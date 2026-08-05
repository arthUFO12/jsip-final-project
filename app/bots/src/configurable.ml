open! Core
open! Async
open Types
open Market_data
open Simulation

let span_of_interval : Time_series.Interval.t -> Time_ns.Span.t = function
  | Minute -> Time_ns.Span.of_min 1.
  | Hour -> Time_ns.Span.of_hr 1.
  | Day -> Time_ns.Span.of_day 1.
;;

module Config = struct
  type t =
    { id : int
    ; start : Time_ns.t
    ; finish : Time_ns.t
    ; interval : Time_series.Interval.t
    ; sim_start_offset : Time_ns.Span.t
    ; initial_cash : Price.t
    ; program : Parser.Program.t
    }
  [@@deriving fields ~getters]

  let create
    ~id
    ~start
    ~finish
    ~interval
    ~sim_start_offset
    ~initial_cash
    ~slugs
    ~rules
    =
    let open Or_error.Let_syntax in
    let%bind program =
      Parser.Program.create ~rules ~tick:(span_of_interval interval) ~slugs
    in
    return
      { id
      ; start
      ; finish
      ; interval
      ; sim_start_offset
      ; initial_cash
      ; program
      }
  ;;
end

module Bot : Bot_interface.Bot with type Config.t = Config.t = struct
  module Config = Config

  module State = struct
    type t =
      { mutable now : Time_ns.t
      ; mutable tick_index : int (** -1 until the first [on_tick]. *)
      ; mutable cash : float
      ; mutable realized : float
      ; mutable unrealized : float
      ; pending_triggers : Parser.Rule.Trigger.Hash_set.t
      (** Accepted fills from the previous tick's [on_response]; consumed by
          WHEN-I gating on the next [on_tick]. *)
      ; mutable inventories : Size.t Slug.Table.t
      (** Positions as of the last [on_response]; empty (= flat) before the
          first fill report. *)
      ; mutable average_costs : Price.t Slug.Table.t
      (** Average price paid per contract of each open position as of the
          last [on_response]; empty before the first fill report. *)
      ; generator : Action.Generator.t
      }

    let forward_time t now = t.now <- now
  end

  module Data = struct
    (* Per-slug price history, oldest first; the last element is the current
       tick. Grown in place by [update_data] only — the future part of the
       series the harness hands to [init_data] is never read, so rules cannot
       look ahead. *)
    type t = Time_series.Point.t Queue.t Slug.Table.t
  end

  let name = "configurable"

  let init_data series (_ : Config.t) : Data.t =
    Hashtbl.keys series
    |> List.map ~f:(fun slug -> slug, Queue.create ())
    |> Slug.Table.of_alist_exn
  ;;

  let update_data (_ : Config.t) (data : Data.t) points : Data.t =
    Hashtbl.iteri points ~f:(fun ~key:slug ~data:point ->
      Queue.enqueue
        (Hashtbl.find_or_add data slug ~default:Queue.create)
        point);
    data
  ;;

  let create_state (config : Config.t) : State.t =
    { now = config.start
    ; tick_index = -1
    ; cash = Price.to_dollar_float config.initial_cash
    ; realized = 0.
    ; unrealized = 0.
    ; pending_triggers = Parser.Rule.Trigger.Hash_set.create ()
    ; inventories = Slug.Table.create ()
    ; average_costs = Slug.Table.create ()
    ; generator = Action.Generator.create ()
    }
  ;;

  let on_start (_ : Config.t) (_ : State.t) = ()

  let eval_env (config : Config.t) (state : State.t) (data : Data.t)
    : Parser.Eval_env.t
    =
    let tick_ns =
      Time_ns.Span.to_int_ns (span_of_interval config.interval)
    in
    let price_ago ~slug ~contract ~ago =
      match Hashtbl.find data slug with
      | None -> None
      | Some history ->
        (* Program validation guarantees [ago] is a whole number of ticks, so
           the division is exact. *)
        let steps = Time_ns.Span.to_int_ns ago / tick_ns in
        let index = Queue.length history - 1 - steps in
        (match index >= 0 with
         | false -> None
         | true ->
           let point = Queue.get history index in
           let price =
             match (contract : Contract_type.t) with
             | Yes -> point.yes_price
             | No -> point.no_price
           in
           Some (Price.to_dollar_float price))
    in
    let inventory ~slug =
      match Hashtbl.find state.inventories slug with
      | None -> 0.
      | Some size -> Float.of_int (Size.to_int size)
    in
    let avg_cost ~slug =
      match Hashtbl.find state.average_costs slug with
      | None -> 0.
      | Some price -> Price.to_dollar_float price
    in
    { cash = state.cash
    ; realized = state.realized
    ; unrealized = state.unrealized
    ; price_ago
    ; inventory
    ; avg_cost
    }
  ;;

  let qualifiers_pass
    (state : State.t)
    ({ rule; every_ticks } : Parser.Program.Compiled_rule.t)
    =
    let every_passes =
      match every_ticks with
      | None -> true
      | Some n -> state.tick_index % n = 0
    in
    let when_i_passes =
      match rule.when_i with
      | None -> true
      | Some trigger -> Hash_set.mem state.pending_triggers trigger
    in
    every_passes && when_i_passes
  ;;

  (* The user-facing "why this fired": the rule's qualifiers first, then the
     true conditions {!Parser.Rule.eval_justified} collected. *)
  let reason_of_rule (rule : Parser.Rule.t) atoms =
    let every =
      match rule.every with
      | None -> []
      | Some span -> [ [%string "every %{span#Time_ns.Span}"] ]
    in
    let when_i =
      match rule.when_i with
      | None -> []
      | Some { side; slug } ->
        let side = match side with Side.Buy -> "buy" | Sell -> "sell" in
        let name = Parser.Ticker_name.normalize (Slug.to_string slug) in
        [ [%string "when i %{side} %{name}"] ]
    in
    let atoms = List.map atoms ~f:Parser.Expr.Atom.to_string in
    match every @ when_i @ atoms with
    | [] -> None
    | phrases -> Some (String.concat phrases ~sep:"; ")
  ;;

  let action_of_spec
    (state : State.t)
    eval
    ?reason
    ({ side; size; slug; contract } : Parser.Rule.Action_spec.t)
    =
    let size = Parser.Expr.Eval.num eval size in
    match Float.iround_nearest size with
    | None ->
      (* nan or infinite size: nothing sensible to trade. *)
      None
    | Some size ->
      (* A negative size is emitted as-is; the harness rejects it and the
         rejection is logged in [on_response], keeping one code path for
         every illegal trade. *)
      Some
        (Action.Generator.new_action
           state.generator
           ?reason
           ~size:(Size.of_int size)
           ~side
           ~slug
           ~contract_type:contract
           ())
  ;;

  let on_tick (config : Config.t) (state : State.t) (data : Data.t) =
    state.tick_index <- state.tick_index + 1;
    let eval = Parser.Expr.Eval.create (eval_env config state data) in
    let actions =
      Parser.Program.rules config.program
      |> List.filter_map ~f:(fun compiled ->
        match qualifiers_pass state compiled with
        | false -> None
        | true ->
          Parser.Rule.eval_justified compiled.rule eval
          |> Option.bind ~f:(fun (spec, atoms) ->
            action_of_spec
              state
              eval
              ?reason:(reason_of_rule compiled.rule atoms)
              spec))
    in
    Hash_set.clear state.pending_triggers;
    actions
  ;;

  let on_response
    (_ : Config.t)
    (state : State.t)
    (responses : Action_response.t list)
    ({ cash; realized_pnl; unrealized_pnl; inventory; average_costs } :
      Action_summary.t)
    =
    List.iter responses ~f:(fun response ->
      match response with
      | Action_accepted { action; _ } ->
        Hash_set.add
          state.pending_triggers
          { side = action.side; slug = action.slug }
      | Action_rejected { action = _; time_stamp = _; reason = _ } -> ());
    state.cash <- Price.to_dollar_float cash;
    state.realized <- Price.to_dollar_float realized_pnl;
    state.unrealized <- Price.to_dollar_float unrealized_pnl;
    (* [Pnl.inventories] and [Pnl.average_cost_bases] build fresh tables per
       call, so keeping the received ones does not alias the harness's book. *)
    state.inventories <- inventory;
    state.average_costs <- average_costs
  ;;
end
