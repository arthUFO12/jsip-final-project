open! Core
open Types

let builtins = [ "$cash"; "$realized"; "$unrealized" ]

let is_name_char c =
  Char.is_alphanum c || Char.equal c '_' || Char.equal c '-'
;;

(* A definition line is [name = ...] with a single '=' (not '=='); mirrors
   the parser's statement detection loosely — a misparse here only costs a
   suggestion, never correctness. *)
let defined_variables program =
  String.split_lines program
  |> List.filter_map ~f:(fun line ->
    match String.lsplit2 line ~on:'=' with
    | None -> None
    | Some (_, after) when String.is_prefix after ~prefix:"=" -> None
    | Some (before, _) ->
      let name = String.strip before in
      (match
         (not (String.is_empty name))
         && String.for_all name ~f:is_name_char
         && (Char.is_alpha name.[0] || Char.equal name.[0] '_')
       with
       | true -> Some [%string "$%{name}"]
       | false -> None))
;;

let env ~tickers ~program =
  Autocomplete.of_list
    (Parser.Ticker_name.keywords
     @ builtins
     @ List.map tickers ~f:(fun ticker ->
       Parser.Ticker_name.normalize (Slug.to_string ticker))
     @ defined_variables program)
;;

let current_token text =
  let is_token_char c = is_name_char c || Char.equal c '$' in
  let length = String.length text in
  let rec start i =
    match i > 0 && is_token_char text.[i - 1] with
    | true -> start (i - 1)
    | false -> i
  in
  let start = start length in
  String.sub text ~pos:start ~len:(length - start)
;;

let complete text ~with_ =
  let token = current_token text in
  String.drop_suffix text (String.length token) ^ with_ ^ " "
;;
