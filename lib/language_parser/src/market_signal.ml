open! Core
open Types

module Direction = struct
  type t =
    | Up
    | Down
  [@@deriving sexp_of, compare, equal, hash]
end

module Magnitude = struct
  type t =
    | Amount of float
    | Percent of float
  [@@deriving sexp_of, compare, equal, hash]
end

module Window = struct
  type t =
    { start_ago : Time_ns.Span.t
    ; end_ago : Time_ns.Span.t
    }
  [@@deriving sexp_of, compare, equal, hash]

  let since span = { start_ago = span; end_ago = Time_ns.Span.zero }
end

module T = struct
  type t =
    | Moved of
        { slug : Slug.t
        ; contract : Contract_type.t
        ; direction : Direction.t
        ; window : Window.t
        ; by : Magnitude.t
        }
    | Above of
        { slug : Slug.t
        ; contract : Contract_type.t
        ; threshold : float
        }
    | Below of
        { slug : Slug.t
        ; contract : Contract_type.t
        ; threshold : float
        }
  [@@deriving sexp_of, compare, equal, hash]
end

include T
include Hashable.Make_plain (T)

let slug = function
  | Moved { slug; _ } | Above { slug; _ } | Below { slug; _ } -> slug
;;

(* %g keeps 0.05 as "0.05" and 5. as "5" — the spellings a program uses. *)
let float_string value = sprintf "%g" value

let to_string t =
  let name slug = Ticker_name.normalize (Slug.to_string slug) in
  match t with
  | Above { slug; contract = _; threshold } ->
    [%string "%{name slug} above %{float_string threshold}"]
  | Below { slug; contract = _; threshold } ->
    [%string "%{name slug} below %{float_string threshold}"]
  | Moved { slug; contract = _; direction; window; by } ->
    let direction =
      match direction with Direction.Up -> "up" | Down -> "down"
    in
    let by =
      match by with
      | Magnitude.Amount amount -> float_string amount
      | Percent pct -> [%string "%{float_string pct}%"]
    in
    let window_end =
      match Time_ns.Span.equal window.end_ago Time_ns.Span.zero with
      | true -> ""
      | false -> [%string " end %{window.end_ago#Time_ns.Span} ago"]
    in
    [%string
      "%{name slug} %{direction} by %{by} since \
       %{window.start_ago#Time_ns.Span} ago%{window_end}"]
;;

let current_price (env : Eval_env.t) ~slug ~contract =
  env.price_ago ~slug ~contract ~ago:Time_ns.Span.zero
;;

(* Prices are floats derived from microcent-resolution [Price.t] (1e-8
   dollars), so an "at least" check on an exact boundary — $0.40 to $0.50 is
   not a representable 0.10 move — needs a tolerance well below price
   resolution and well above rounding noise. *)
let boundary_tolerance = 1e-9

let evaluate (t : t) ~(env : Eval_env.t) : bool =
  match t with
  | Above { slug; contract; threshold } ->
    (match current_price env ~slug ~contract with
     | None -> false
     | Some price -> Float.O.(price > threshold))
  | Below { slug; contract; threshold } ->
    (match current_price env ~slug ~contract with
     | None -> false
     | Some price -> Float.O.(price < threshold))
  | Moved { slug; contract; direction; window; by } ->
    (match
       ( env.price_ago ~slug ~contract ~ago:window.start_ago
       , env.price_ago ~slug ~contract ~ago:window.end_ago )
     with
     | None, _ | _, None -> false
     | Some start_price, Some end_price ->
       let change =
         match direction with
         | Up -> end_price -. start_price
         | Down -> start_price -. end_price
       in
       let at_least required =
         Float.O.(change +. boundary_tolerance >= required)
       in
       (match by with
        | Amount amount -> at_least amount
        | Percent pct ->
          (* A percent move off a zero start price is undefined. *)
          Float.O.(start_price > 0.) && at_least (start_price *. pct /. 100.)))
;;
