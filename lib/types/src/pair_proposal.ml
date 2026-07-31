open! Core

module Status = struct
  type t =
    | Proposed
    | Approved
    | Rejected
  [@@deriving sexp_of, equal, string]
end

type t =
  { left : Market_id.t
  ; right : Market_id.t
  ; score : float
  ; explanation : string option
  ; status : Status.t
  }
[@@deriving sexp_of]

let create ~left ~right ~score ~explanation =
  let left, right =
    if Market_id.( <= ) left right then left, right else right, left
  in
  { left; right; score; explanation; status = Proposed }
;;
