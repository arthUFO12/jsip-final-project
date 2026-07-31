open! Core
open! Types

module Candidate = struct
  type t =
    { left : Market_stub.t
    ; right : Market_stub.t
    }
  [@@deriving sexp_of]
end

(* On app startup do some preliminary work for this and other elements *)

(* Different names for the same thing across venues, folded to one spelling
   during normalization so "Bitcoin above 100k" and "BTC above $100k" compare
   equal everywhere downstream (trigrams, tags, jaccard). Extend as real
   titles surface new aliases. *)
let aliases =
  String.Map.of_alist_exn
    [ "bitcoin", "btc"
    ; "ethereum", "eth"
    ; "solana", "sol"
    ; "dogecoin", "doge"
    ; "federal", "fed"
    ]
;;

(* Lowercase, turn punctuation into spaces, squeeze runs of whitespace, and
   fold aliases, so "Will Bitcoin hit $100k?" and "will btc hit 100k"
   normalize identically. *)
let normalize text =
  String.lowercase text
  |> String.map ~f:(fun c -> if Char.is_alphanum c then c else ' ')
  |> String.split ~on:' '
  |> List.filter ~f:(fun word -> not (String.is_empty word))
  |> List.map ~f:(fun word ->
    Option.value (Map.find aliases word) ~default:word)
  |> String.concat ~sep:" "
;;

(* Character trigrams of the normalized text, e.g. "btc up" ->
   [{"btc"; "tc "; "c u"; " up"}]. Strings shorter than three characters
   become a single gram so short titles still compare non-trivially. *)
let trigrams text =
  let text = normalize text in
  let len = String.length text in
  if len = 0
  then String.Set.empty
  else if len <= 3
  then String.Set.singleton text
  else
    String.Set.of_list
      (List.init (len - 2) ~f:(fun pos -> String.sub text ~pos ~len:3))
;;

(* |A ∩ B| / |A ∪ B| over trigram sets: 1.0 for identical normalized text,
   0.0 for no overlap (including when either side is empty). *)
let jaccard left right =
  let left = trigrams left in
  let right = trigrams right in
  let union = Set.union left right in
  if Set.is_empty union
  then 0.
  else
    Float.of_int (Set.length (Set.inter left right))
    /. Float.of_int (Set.length union)
;;

let score (candidate : Candidate.t) =
  jaccard candidate.left.title candidate.right.title
;;

(* Words so common in market titles that sharing only them says nothing about
   whether two markets are the same bet. *)
let stopwords =
  String.Set.of_list
    [ "will"
    ; "the"
    ; "a"
    ; "an"
    ; "in"
    ; "on"
    ; "by"
    ; "at"
    ; "to"
    ; "of"
    ; "be"
    ; "before"
    ; "after"
    ; "and"
    ; "or"
    ; "yes"
    ; "no"
    ]
;;

(* The blocking tags of a title: its normalized words, minus stopwords and
   single characters. Two markets are only compared if they share a tag. *)
let tags title =
  normalize title
  |> String.split ~on:' '
  |> List.filter ~f:(fun word ->
    String.length word > 1 && not (Set.mem stopwords word))
  |> String.Set.of_list
;;

(* Two markets in different known categories are never the same event. A
   Miscellaneous label only means the venue's taxonomy didn't map to ours, so
   it blocks nothing — the venues categorize independently, and a real pair
   must survive one side being unlabeled. *)
let categories_compatible left right =
  Category.equal left right
  || Category.equal left Category.Miscellaneous
  || Category.equal right Category.Miscellaneous
;;

(* Pair each left market with only the right markets in a compatible category
   sharing at least one tag, using an index from tag to right markets so we
   never walk the full cross product. Each surviving pair appears once. *)
let block lefts rights =
  let index =
    List.concat_map rights ~f:(fun (right : Market_stub.t) ->
      Set.to_list (tags right.title) |> List.map ~f:(fun tag -> tag, right))
    |> String.Map.of_alist_multi
  in
  List.concat_map lefts ~f:(fun (left : Market_stub.t) ->
    Set.to_list (tags left.title)
    |> List.concat_map ~f:(fun tag ->
      Option.value (Map.find index tag) ~default:[])
    |> List.dedup_and_sort
         ~compare:
           (Comparable.lift
              Market_id.compare
              ~f:(fun (right : Market_stub.t) -> right.market_id))
    |> List.filter ~f:(fun (right : Market_stub.t) ->
      categories_compatible left.category right.category)
    |> List.map ~f:(fun right -> { Candidate.left; right }))
;;

(* Maximal digit runs in the raw title, e.g. "BTC above $100,000?" ->
   [{"100"; "000"}]. Comparing runs (rather than parsed values) sidesteps
   separators and suffixes like "100k". *)
let numbers text =
  String.to_list text
  |> List.group ~break:(fun a b ->
    Bool.( <> ) (Char.is_digit a) (Char.is_digit b))
  |> List.filter_map ~f:(fun group ->
    if List.for_all group ~f:Char.is_digit
    then Some (String.of_char_list group)
    else None)
  |> String.Set.of_list
;;

(* Markets resolving more than this far apart are not the same event, no
   matter how similar the titles read. *)
let close_time_tolerance = Time_ns.Span.of_day 1.

let veto ({ left; right } : Candidate.t) =
  let left_numbers = numbers left.title in
  let right_numbers = numbers right.title in
  let numbers_conflict =
    (not (Set.is_empty left_numbers))
    && (not (Set.is_empty right_numbers))
    && Set.is_empty (Set.inter left_numbers right_numbers)
  in
  if numbers_conflict
  then (
    let show set = String.concat ~sep:" " (Set.to_list set) in
    Some
      [%string
        "titles disagree on numbers: [%{show left_numbers}] vs [%{show \
         right_numbers}]"])
  else (
    match left.close_time, right.close_time with
    | None, None | None, Some _ | Some _, None -> None
    | Some left_time, Some right_time ->
      let gap = Time_ns.abs_diff left_time right_time in
      if Time_ns.Span.( > ) gap close_time_tolerance
      then Some [%string "close times %{gap#Time_ns.Span} apart"]
      else None)
;;

let find_candidates ~threshold ~apply_veto lefts rights =
  block lefts rights
  |> List.filter ~f:(fun candidate ->
    Float.( >= ) (score candidate) threshold)
  |> List.filter ~f:(fun candidate ->
    (not apply_veto) || Option.is_none (veto candidate))
;;

module For_testing = struct
  let normalize = normalize
  let trigrams = trigrams
  let jaccard = jaccard
  let score = score
  let block = block
  let veto = veto
end
