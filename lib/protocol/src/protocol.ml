open! Core
open Async_rpc_kernel
open Types

let epoch_seconds time =
  Time_ns.to_span_since_epoch time |> Time_ns.Span.to_sec
;;

module Market_card = struct
  module Volume = struct
    (* [Types.Volume.t] carries a microcent [Price.t], which overflows the
       browser's 32-bit ints past ~$21 — so notionals cross the wire as float
       dollars, per the money convention in the mli header. *)
    type t =
      | Contracts of int
      | Notional_dollars of float
    [@@deriving sexp_of, bin_io, compare, equal]

    let of_domain : Types.Volume.t -> t = function
      | Contracts size -> Contracts (Size.to_int size)
      | Notional price -> Notional_dollars (Price.to_dollar_float price)
    ;;

    let to_float = function
      | Contracts count -> Float.of_int count
      | Notional_dollars dollars -> dollars
    ;;
  end

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
    ; volume = Option.map stub.volume ~f:Volume.of_domain
    ; has_price_history = Market_stub.can_fetch_history stub
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

module Market_detail = struct
  type t =
    { prices : (float * float) list
    ; volumes : (float * float) list
    }
  [@@deriving sexp_of, bin_io]
end

module Basic_bots = struct
  type t =
    { count : int
    ; trade_probability : float
    ; max_size : int
    }
  [@@deriving sexp_of, bin_io]
end

module Rival_bot = struct
  type t =
    { name : string
    ; slugs : Slug.t list
    ; program : string
    ; variables : string list
    }
  [@@deriving sexp_of, bin_io]
end

module Comparison = struct
  type t =
    | No_comparison
    | Dumb_bots of Basic_bots.t
    | Rival_bot of Rival_bot.t
  [@@deriving sexp_of, bin_io]
end

module Sim_request = struct
  type t =
    { slugs : Slug.t list
    ; program : string
    ; variables : string list
    ; interval : Interval.t
    ; lookback_days : int
    ; warmup_hours : int
    ; starting_cash_cents : int
    ; allow_negative_cash : bool
    ; comparison : Comparison.t
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
    ; reason : string option
    (** Why the bot fired this action: qualifiers plus the true conditions. *)
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
    ; inventory : (Slug.t * int) list
    }
  [@@deriving sexp_of, bin_io]
end

module Baseline_point = struct
  type t =
    { time_s : float
    ; cash : float
    ; realized : float
    ; unrealized : float
    ; portfolio_value : float
    }
  [@@deriving sexp_of, bin_io]
end

module Sim_result = struct
  type t =
    { ticks : Tick_point.t list
    ; fills : Fill.t list
    ; sim_start_s : float
    ; baseline_ticks : Baseline_point.t list
    ; pnl_percentile : float option
    ; truncated : bool
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

module Pair_counts = struct
  type t =
    { proposed : int
    ; approved : int
    ; rejected : int
    }
  [@@deriving sexp_of, bin_io]

  let total { proposed; approved; rejected } = proposed + approved + rejected
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
    { tab : Pair_status.t
    ; index : int
    ; new_status : Pair_status.t
    }
  [@@deriving sexp_of, bin_io]
end

module Llm_review_request = struct
  type t = { api_key : string option } [@@deriving sexp_of, bin_io]
end

module Auto_review_request = struct
  type t = { threshold : float } [@@deriving sexp_of, bin_io]
end

module Auto_review_summary = struct
  type t =
    { reviewed : int
    ; approved : int
    ; left_proposed : int
    }
  [@@deriving sexp_of, bin_io]
end

module Llm_review_summary = struct
  type t =
    { reviewed : int
    ; approved : int
    ; rejected : int
    ; errored : int
    ; first_error : string option
    }
  [@@deriving sexp_of, bin_io]
end

module Edge_leg = struct
  type t =
    { venue : string
    ; title : string
    ; ask : float
    ; url : string
    }
  [@@deriving sexp_of, bin_io]
end

module Edge_card = struct
  type t =
    { pair_key : string
    ; yes : Edge_leg.t
    ; no : Edge_leg.t
    ; cost : float
    ; edge : float
    ; size : int
    ; dollars : float
    ; tradable : bool
    ; acted : bool
    }
  [@@deriving sexp_of, bin_io]
end

module Venue_error = struct
  type t =
    | Geo_blocked
    | Insufficient_funds
    | Market_closed_or_halted
    | Rate_limited
    | Other of string
  [@@deriving sexp_of, bin_io, equal]
end

module Execution_capability = struct
  type t =
    { live : bool
    ; reason : string option
    }
  [@@deriving sexp_of, bin_io]
end

module Preflight_request = struct
  type t =
    { pair_key : string
    ; manual_is_yes : bool
    }
  [@@deriving sexp_of, bin_io]
end

module Preflight = struct
  type t =
    { kill_switch : string option
    ; hedge_title : string
    ; hedge_is_yes : bool
    ; ask_cents : int
    ; depth : int
    ; caps_room_dollars : float
    ; hedgeable : int
    }
  [@@deriving sexp_of, bin_io]
end

module Hedge_request = struct
  type t =
    { pair_key : string
    ; nonce : string
    ; manual_venue : string
    ; manual_is_yes : bool
    ; manual_price_cents : int
    ; manual_count : int
    ; max_hedge_price_cents : int
    }
  [@@deriving sexp_of, bin_io]
end

module Hedge_result = struct
  type t =
    | Hedged of
        { price : float
        ; count : int
        ; fee : float
        ; unhedged : int
        }
    | Refused of string
    | Failed of
        { error : Venue_error.t
        ; detail : string
        ; unhedged : int
        }
  [@@deriving sexp_of, bin_io]
end

module Wallet_card = struct
  type t =
    { pair_key : string
    ; summary : string
    ; dollars : float
    ; acted : bool
    ; acted_dollars : float
    }
  [@@deriving sexp_of, bin_io]
end

module Wallet = struct
  type t =
    { paper : float
    ; acted : float
    ; entries : Wallet_card.t list
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

module Trading_key = struct
  module Status = struct
    type t =
      { connected : bool
      ; unlocked : bool
      ; key_hint : string option
      ; production : bool
      ; live_allowed : bool
      }
    [@@deriving sexp_of, bin_io]
  end

  module Connect_request = struct
    type t =
      { key_id : string
      ; private_key_pem : string
      ; passphrase : string
      ; production : bool
      }
    [@@deriving sexp_of, bin_io]
  end
end

module Account = struct
  module Position = struct
    type t =
      { ticker : string
      ; position : int
      ; exposure_dollars : float
      }
    [@@deriving sexp_of, bin_io]
  end

  type t =
    { balance_dollars : float
    ; positions : Position.t list
    ; production : bool
    }
  [@@deriving sexp_of, bin_io]
end

let get_markets =
  Rpc.Rpc.create
    ~name:"get-markets"
    ~version:1
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Market_card.t list Or_error.t]
    ~include_in_error_count:Or_error
;;

let get_market_detail =
  Rpc.Rpc.create
    ~name:"get-market-detail"
    ~version:0
    ~bin_query:[%bin_type_class: Slug.t]
    ~bin_response:[%bin_type_class: Market_detail.t Or_error.t]
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

let get_pair_counts =
  Rpc.Rpc.create
    ~name:"get-pair-counts"
    ~version:0
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Pair_counts.t Or_error.t]
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

let llm_review_pairs =
  Rpc.Rpc.create
    ~name:"llm-review-pairs"
    ~version:0
    ~bin_query:[%bin_type_class: Llm_review_request.t]
    ~bin_response:[%bin_type_class: Llm_review_summary.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let auto_review_pairs =
  Rpc.Rpc.create
    ~name:"auto-review-pairs"
    ~version:0
    ~bin_query:[%bin_type_class: Auto_review_request.t]
    ~bin_response:[%bin_type_class: Auto_review_summary.t Or_error.t]
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

let get_execution_capability =
  Rpc.Rpc.create
    ~name:"get-execution-capability"
    ~version:0
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Execution_capability.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let preflight_hedge =
  Rpc.Rpc.create
    ~name:"preflight-hedge"
    ~version:0
    ~bin_query:[%bin_type_class: Preflight_request.t]
    ~bin_response:[%bin_type_class: Preflight.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let execute_hedge =
  Rpc.Rpc.create
    ~name:"execute-hedge"
    ~version:0
    ~bin_query:[%bin_type_class: Hedge_request.t]
    ~bin_response:[%bin_type_class: Hedge_result.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let get_wallet =
  Rpc.Rpc.create
    ~name:"get-wallet"
    ~version:0
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Wallet.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let mark_acted =
  Rpc.Rpc.create
    ~name:"mark-acted"
    ~version:0
    ~bin_query:[%bin_type_class: string]
    ~bin_response:[%bin_type_class: Wallet.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let run_simulation =
  Rpc.Rpc.create
    ~name:"run-simulation"
    ~version:6
    ~bin_query:[%bin_type_class: Sim_request.t]
    ~bin_response:[%bin_type_class: Sim_result.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let get_trading_key =
  Rpc.Rpc.create
    ~name:"get-trading-key"
    ~version:0
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Trading_key.Status.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let connect_trading_key =
  Rpc.Rpc.create
    ~name:"connect-trading-key"
    ~version:0
    ~bin_query:[%bin_type_class: Trading_key.Connect_request.t]
    ~bin_response:[%bin_type_class: Trading_key.Status.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let unlock_trading_key =
  Rpc.Rpc.create
    ~name:"unlock-trading-key"
    ~version:0
    ~bin_query:[%bin_type_class: string]
    ~bin_response:[%bin_type_class: Trading_key.Status.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let lock_trading_key =
  Rpc.Rpc.create
    ~name:"lock-trading-key"
    ~version:0
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Trading_key.Status.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let get_account =
  Rpc.Rpc.create
    ~name:"get-account"
    ~version:0
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Account.t Or_error.t]
    ~include_in_error_count:Or_error
;;

let trip_kill_switch =
  Rpc.Rpc.create
    ~name:"trip-kill-switch"
    ~version:0
    ~bin_query:[%bin_type_class: string]
    ~bin_response:[%bin_type_class: string Or_error.t]
    ~include_in_error_count:Or_error
;;

let forget_trading_key =
  Rpc.Rpc.create
    ~name:"forget-trading-key"
    ~version:0
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Trading_key.Status.t Or_error.t]
    ~include_in_error_count:Or_error
;;
