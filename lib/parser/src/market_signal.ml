open! Core

module Direction = struct
  type t =
    | Up
    | Down
  [@@deriving sexp, bin_io, compare, equal, hash]
end

type t =
  | Change_pct of
      { arg_name : string
      ; direction : Direction.t
      ; start_days_ago : int
      ; end_days_ago : int
      ; pct : float
      }
  | Change_absolute of
      { arg_name : string
      ; direction : Direction.t
      ; start_days_ago : int
      ; end_days_ago : int
      ; amt : float
      }
[@@deriving sexp, bin_io, compare, equal, hash]

let evaluate_signal (t : t) _b =
  ignore t;
  true
;;
