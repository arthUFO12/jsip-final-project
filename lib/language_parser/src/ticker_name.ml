open! Core
open Types

let keywords =
  [ "if"
  ; "then"
  ; "else"
  ; "every"
  ; "when"
  ; "i"
  ; "buy"
  ; "sell"
  ; "yes"
  ; "no"
  ; "up"
  ; "down"
  ; "by"
  ; "of"
  ; "since"
  ; "end"
  ; "ago"
  ; "price"
  ; "inventory"
  ; "avgcost"
  ; "true"
  ; "false"
  ]
;;

let normalize word =
  String.lowercase word
  |> String.map ~f:(fun c -> if Char.equal c '-' then '_' else c)
;;

(* Must agree with {!Token}'s word rules: a name that would not lex as one
   [Word] could never be typed back in a program. *)
let is_name_start c = Char.is_alpha c || Char.equal c '_'
let is_name_char c = Char.is_alphanum c || Char.equal c '_'

let validate_name ~slug name =
  match String.to_list name with
  | [] -> Or_error.error_s [%message "market has an empty ticker"]
  | first :: rest ->
    (match is_name_start first && List.for_all rest ~f:is_name_char with
     | false ->
       Or_error.error_s
         [%message
           "market ticker cannot be written as a program name"
             (slug : Slug.t)
             ~normalized:(name : string)]
     | true ->
       (match List.mem keywords name ~equal:String.equal with
        | true ->
          Or_error.error_s
            [%message
              "market ticker collides with a language keyword"
                (slug : Slug.t)
                ~keyword:(name : string)]
        | false -> Ok ()))
;;

let build_map slugs =
  List.fold_result slugs ~init:String.Map.empty ~f:(fun map slug ->
    let name = normalize (Slug.to_string slug) in
    let%bind.Or_error () = validate_name ~slug name in
    match Map.add map ~key:name ~data:slug with
    | `Ok map -> Ok map
    | `Duplicate ->
      Or_error.error_s
        [%message
          "two market tickers normalize to the same name"
            ~name:(name : string)
            (slug : Slug.t)
            ~clashes_with:(Map.find_exn map name : Slug.t)])
;;
