open! Core
open! Async
open Types
open Arbitrage

(* The server half of the arbitrage pipeline: each function backs one
   {!Protocol} RPC by delegating to the same {!Arbitrage} modules the CLI
   verbs use — [Sweep] files proposals, [Review] lists and decides them, and
   one detection tick prices the approved set like {!Bot} would, without
   placing orders. Wire conversions (status mirror, float dollars) all live
   here, at the boundary. *)

let status_of_wire : Protocol.Pair_status.t -> Pair_proposal.Status.t
  = function
  | Proposed -> Proposed
  | Approved -> Approved
  | Rejected -> Rejected
;;

let status_to_wire : Pair_proposal.Status.t -> Protocol.Pair_status.t
  = function
  | Proposed -> Proposed
  | Approved -> Approved
  | Rejected -> Rejected
;;

let microcents_per_dollar = 100_000_000.

let dollars price =
  Float.of_int (Price.to_microcents price) /. microcents_per_dollar
;;

let card_of_listed ({ index; proposal; left; right } : Review.Listed.t)
  : Protocol.Pair_card.t
  =
  let title_and_venue stub market_id =
    match (stub : Market_stub.t option) with
    | Some stub -> stub.title, Venue.to_string stub.venue
    | None -> [%string "<no stub for %{market_id#Market_id}>"], "?"
  in
  let left_title, left_venue = title_and_venue left proposal.left in
  let right_title, right_venue = title_and_venue right proposal.right in
  { index
  ; left_title
  ; left_venue
  ; right_title
  ; right_venue
  ; score = proposal.score
  ; explanation = proposal.explanation
  ; status = status_to_wire proposal.status
  }
;;

let list_pairs status =
  let open Deferred.Or_error.Let_syntax in
  let%map listed = Review.list_by_status (status_of_wire status) in
  List.map listed ~f:card_of_listed
;;

let decide ({ index; approve } : Protocol.Decide_request.t) =
  let open Deferred.Or_error.Let_syntax in
  let status : Pair_proposal.Status.t =
    if approve then Approved else Rejected
  in
  let%map decided = Review.decide ~index ~status in
  card_of_listed decided
;;

let sweep ({ threshold } : Protocol.Sweep_request.t) =
  let open Deferred.Or_error.Let_syntax in
  let%map { Sweep.markets_swept
          ; search_hits
          ; fresh_candidates = _
          ; proposed
          ; auto_rejected = _
          }
    =
    Sweep.sweep_full
      ~matching:(Text_only { threshold })
      ~max_adjudications:0
      ()
  in
  { Protocol.Sweep_summary.markets_swept; search_hits; proposed }
;;

(* The bot's own execution defaults decide what counts as tradable, so the
   page and the paper bot never disagree about a hit. *)
let execution_defaults = Config.Execution.default

(* Price one split of a pair — buy YES on [yes], NO on [no] — into the wire
   card the page renders. [cost] carries the fees {!Detect} charges; the
   per-leg [ask] shown next to each venue is the raw book price. *)
let card_of_split
  ~(yes_stub : Market_stub.t)
  ~(no_stub : Market_stub.t)
  ~(yes : Detect.Leg.t)
  ~(no : Detect.Leg.t)
  =
  let cost, size = Detect.cost_and_size ~mode:Exact ~yes ~no in
  let cost = dollars cost in
  let edge = 1. -. cost in
  { Protocol.Edge_card.yes =
      { venue = Venue.to_string yes.venue
      ; title = yes_stub.title
      ; ask = dollars yes.book.yes_ask
      }
  ; no =
      { venue = Venue.to_string no.venue
      ; title = no_stub.title
      ; ask = dollars no.book.no_ask
      }
  ; cost
  ; edge
  ; size = Size.to_int size
  ; tradable =
      Float.( >= ) edge (dollars execution_defaults.min_edge)
      && Size.to_int size > 0
  }
;;

let edge_card ~legs ({ left; right } : Matcher.Candidate.t) =
  let leg_of (stub : Market_stub.t) =
    List.find legs ~f:(fun (leg : Detect.Leg.t) ->
      Market_id.equal leg.market_id stub.market_id)
  in
  match leg_of left, leg_of right with
  | None, None | None, Some _ | Some _, None -> None
  | Some left_leg, Some right_leg ->
    let card_a =
      card_of_split ~yes_stub:left ~no_stub:right ~yes:left_leg ~no:right_leg
    in
    let card_b =
      card_of_split ~yes_stub:right ~no_stub:left ~yes:right_leg ~no:left_leg
    in
    Some (if Float.( <= ) card_a.cost card_b.cost then card_a else card_b)
;;

let scan () =
  let open Deferred.Or_error.Let_syntax in
  let%bind candidates = Bot.candidates_of_approved () in
  let%bind legs = Deferred.ok (Bot.fetch_legs candidates) in
  let edges = List.filter_map candidates ~f:(edge_card ~legs) in
  return
    { Protocol.Scan_report.pairs = List.length candidates
    ; legs_priced = List.length legs
    ; edges
    ; tradable =
        List.count edges ~f:(fun edge -> edge.Protocol.Edge_card.tradable)
    }
;;
