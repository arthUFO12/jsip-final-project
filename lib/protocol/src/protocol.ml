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

module Pair_status = struct
  type t =
    | Proposed
    | Approved
    | Rejected
  [@@deriving sexp_of, bin_io, compare, equal, enumerate]

  let name = function
    | Proposed -> "proposed"
    | Approved -> "approved"
    | Rejected -> "rejected"
  ;;
end

module Pair_card = struct
  type t =
    { index : int
    ; left_title : string
    ; left_venue : string
    ; right_title : string
    ; right_venue : string
    ; score : float
    ; explanation : string option
    ; status : Pair_status.t
    }
  [@@deriving sexp_of, bin_io]
end

module Sweep_request = struct
  type t = { threshold : float } [@@deriving sexp_of, bin_io]
end

module Sweep_summary = struct
  type t =
    { markets_swept : int
    ; search_hits : int
    ; proposed : int
    }
  [@@deriving sexp_of, bin_io]
end

module Decide_request = struct
  type t =
    { index : int
    ; approve : bool
    }
  [@@deriving sexp_of, bin_io]
end

module Edge_leg = struct
  type t =
    { venue : string
    ; title : string
    ; ask : float
    }
  [@@deriving sexp_of, bin_io]
end

module Edge_card = struct
  type t =
    { yes : Edge_leg.t
    ; no : Edge_leg.t
    ; cost : float
    ; edge : float
    ; size : int
    ; tradable : bool
    }
  [@@deriving sexp_of, bin_io]
end

module Scan_report = struct
  type t =
    { pairs : int
    ; legs_priced : int
    ; edges : Edge_card.t list
    ; tradable : int
    }
  [@@deriving sexp_of, bin_io]
end

let get_markets =
  Rpc.Rpc.create
    ~name:"get-markets"
    ~version:0
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Market_card.t list Or_error.t]
    ~include_in_error_count:Or_error
;;

let get_pairs =
  Rpc.Rpc.create
    ~name:"get-pairs"
    ~version:0
    ~bin_query:[%bin_type_class: Pair_status.t]
    ~bin_response:[%bin_type_class: Pair_card.t list Or_error.t]
    ~include_in_error_count:Or_error
;;

let decide_pair =
  Rpc.Rpc.create
    ~name:"decide-pair"
    ~version:0
    ~bin_query:[%bin_type_class: Decide_request.t]
    ~bin_response:[%bin_type_class: Pair_card.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let run_sweep =
  Rpc.Rpc.create
    ~name:"run-sweep"
    ~version:0
    ~bin_query:[%bin_type_class: Sweep_request.t]
    ~bin_response:[%bin_type_class: Sweep_summary.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let scan_edges =
  Rpc.Rpc.create
    ~name:"scan-edges"
    ~version:0
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Scan_report.t Or_error.t]
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
