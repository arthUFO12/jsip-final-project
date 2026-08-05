open! Core
open Types

type t =
  { name : string
  ; slugs : Slug.t list
  ; rules : string list
  ; variables : string list
  ; interval : string
  ; lookback : string
  ; warmup : string
  ; starting_cash : string
  ; allow_negative : bool
  }
[@@deriving sexp, equal]

let upsert bots bot =
  List.filter bots ~f:(fun b -> not (String.equal b.name bot.name)) @ [ bot ]
;;

let find bots ~name =
  List.find bots ~f:(fun bot -> String.equal bot.name name)
;;

let remove bots ~name =
  List.filter bots ~f:(fun bot -> not (String.equal bot.name name))
;;

let list_to_string bots = Sexp.to_string ([%sexp_of: t list] bots)

let list_of_string text =
  match
    Or_error.try_with (fun () -> [%of_sexp: t list] (Sexp.of_string text))
  with
  | Ok bots -> bots
  | Error (_ : Error.t) -> []
;;
