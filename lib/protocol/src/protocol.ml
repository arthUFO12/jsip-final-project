open! Core
open Async_rpc_kernel
open Types

let epoch_seconds time =
  Time_ns.to_span_since_epoch time |> Time_ns.Span.to_sec
;;

module Market_card = struct
  type t =
    { slug : Slug.t
    ; title : string
    ; category : Category.t
    ; volume : Volume.t option
    ; has_price_history : bool
    }
  [@@deriving sexp_of, bin_io, compare, equal]

  let of_stub (stub : Market_stub.t) =
    { slug = stub.slug
    ; title = stub.title
    ; category = stub.category
    ; volume = stub.volume
    ; has_price_history = Option.is_some stub.series_ticker
    }
  ;;
end

module Interval = struct
  type t =
    | Minute
    | Hour
    | Day
  [@@deriving sexp_of, bin_io, compare, equal, enumerate]

  let name = function Minute -> "minute" | Hour -> "hour" | Day -> "day"
end

module Sim_request = struct
  type t =
    { slugs : Slug.t list
    ; program : string
    ; interval : Interval.t
    ; lookback_days : int
    ; warmup_hours : int
    }
  [@@deriving sexp_of, bin_io]
end

module Fill = struct
  type t =
    { time_s : float
    ; id : int
    ; side : Side.t
    ; contract : Contract_type.t
    ; size : int
    ; slug : Slug.t
    ; rejected : string option (** [None] means the fill was accepted. *)
    }
  [@@deriving sexp_of, bin_io]
end

module Tick_point = struct
  type t =
    { time_s : float
    ; cash : float
    ; realized : float
    ; unrealized : float
    ; yes_prices : (Slug.t * float) list
    }
  [@@deriving sexp_of, bin_io]
end

module Sim_result = struct
  type t =
    { ticks : Tick_point.t list
    ; fills : Fill.t list
    ; sim_start_s : float
    }
  [@@deriving sexp_of, bin_io]

  let final t = List.last t.ticks
end

let get_markets =
  Rpc.Rpc.create
    ~name:"get-markets"
    ~version:0
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Market_card.t list Or_error.t]
    ~include_in_error_count:Or_error
;;

let run_simulation =
  Rpc.Rpc.create
    ~name:"run-simulation"
    ~version:0
    ~bin_query:[%bin_type_class: Sim_request.t]
    ~bin_response:[%bin_type_class: Sim_result.t Or_error.t]
    ~include_in_error_count:Or_error
;;
