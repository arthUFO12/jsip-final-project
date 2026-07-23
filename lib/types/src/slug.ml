open! Core

module T = struct
  type t = string [@@deriving sexp, bin_io, compare, equal, hash, string]
end

include T
include Hashable.Make (T)

let of_string = Fn.id
let to_string = Fn.id
