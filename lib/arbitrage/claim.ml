open! Core
open! Types

(* One record, six fields. Verdicts come from comparing the two records field
   by field — every false-positive class in the comparison report was one
   missing field, so the fields carry what the old vetoes guessed at.
   Abstention is per pair, per field: an [Unknown] on one field sends that
   one pair back to the string pipeline, never a whole domain. *)

module Field_cmp = struct
  type t =
    | Agree
    | Disagree
    | Unknown
  [@@deriving sexp_of, compare, equal]
end

module Domain = struct
  type t =
    | Rates
    | Championship
    | Award
    | Statistic
    | Fixture
    | Media_rank
  [@@deriving sexp_of, compare, equal, enumerate]

  let to_string t = Sexp.to_string [%sexp (t : t)]

  (* In these domains one subject takes the prize, so two different subjects
     in the same arena are mutually exclusive (Disjoint). Rates and
     statistics are not: the Fed and the BoC can both cut. *)
  let subjects_exclusive = function
    | Championship | Award | Fixture | Media_rank -> true
    | Rates | Statistic -> false
  ;;
end

(* ------------------------------------------------------------------ *)
(* Entities *)
(* ------------------------------------------------------------------ *)
module Entity = struct
  module T = struct
    type t =
      { key : string
      ; resolved : bool
      }
    [@@deriving sexp_of, compare, equal]
  end

  include T

  (* Common Latin diacritics folded to ASCII so Dembele/Dembélé and
     Hojgaard/Højgaard compare equal. *)
  let diacritics =
    [ "á", "a"
    ; "à", "a"
    ; "â", "a"
    ; "ä", "a"
    ; "ã", "a"
    ; "å", "a"
    ; "é", "e"
    ; "è", "e"
    ; "ê", "e"
    ; "ë", "e"
    ; "í", "i"
    ; "ì", "i"
    ; "î", "i"
    ; "ï", "i"
    ; "ó", "o"
    ; "ò", "o"
    ; "ô", "o"
    ; "ö", "o"
    ; "õ", "o"
    ; "ø", "o"
    ; "ú", "u"
    ; "ù", "u"
    ; "û", "u"
    ; "ü", "u"
    ; "ñ", "n"
    ; "ç", "c"
    ; "ý", "y"
    ; "æ", "ae"
    ; "œ", "oe"
    ; "ß", "ss"
    ]
  ;;

  let fold_diacritics text =
    List.fold diacritics ~init:text ~f:(fun text (pattern, with_) ->
      String.substr_replace_all text ~pattern ~with_)
  ;;

  (* Filler that never distinguishes one entity from another. *)
  let noise_words = [ "the"; "a"; "an"; "jr"; "sr"; "fc"; "ks"; "cf"; "sc" ]

  let normalize raw =
    String.lowercase raw
    |> fold_diacritics
    |> String.map ~f:(fun c -> if Char.is_alphanum c then c else ' ')
    |> String.split ~on:' '
    |> List.filter ~f:(fun word ->
      (not (String.is_empty word))
      && not (List.mem noise_words word ~equal:String.equal))
    |> String.concat ~sep:" "
  ;;

  (* canonical key -> other spellings. Seeded from the collision classes the
     comparison report surfaced; extend as new ones appear. Presence here is
     what makes a name [resolved] — resolved-and-different is a confident
     Disagree, unresolved-and-different is only Unknown. *)
  let aliases =
    [ (* central banks *)
      "fed", [ "fomc"; "federal reserve"; "us federal reserve" ]
    ; "ecb", [ "european central bank" ]
    ; "boe", [ "bank of england" ]
    ; "boj", [ "bank of japan" ]
    ; "boc", [ "bank of canada" ]
    ; "bok", [ "bank of korea" ]
    ; "rba", [ "reserve bank of australia" ]
    ; "snb", [ "swiss national bank" ]
    ; (* crypto assets *)
      "btc", [ "bitcoin" ]
    ; "eth", [ "ethereum" ]
    ; "sol", [ "solana" ]
    ; "doge", [ "dogecoin" ]
    ; "xrp", [ "ripple" ]
    ; (* organizations that collide in award markets *)
      "icj", [ "international court of justice" ]
    ; "icc", [ "international criminal court" ]
    ; "openai", [ "open ai" ]
    ; (* competitions (arena entities) *)
      "epl", [ "english premier league"; "premier league" ]
    ; ( "ucl"
      , [ "champions league"
        ; "uefa champions league"
        ; "uefa champions league championship"
        ] )
    ; "f1 drivers", [ "f1 drivers championship"; "f1 drivers champion" ]
    ; ( "f1 constructors"
      , [ "f1 constructors championship"; "f1 constructors champion" ] )
    ; "world chess championship", []
    ; "us open mens", [ "us open men s singles"; "men s us open" ]
    ; "us open womens", [ "us open women s singles"; "women s us open" ]
    ; ( "nl championship"
      , [ "pro baseball national league championship"
        ; "national league championship series"
        ] )
    ; ( "al championship"
      , [ "pro baseball american league championship"
        ; "american league championship series"
        ] )
    ; "stanley cup", [ "stanley cup finals" ]
    ; "nba finals", []
    ; "super bowl", []
    ; "world series", []
    ; "wyndham championship", []
    ; "big brother", [ "big brother season" ]
    ; (* awards *)
      "ballon d or", [ "ballon dor" ]
    ; "nobel peace prize", []
    ; ( "best actress"
      , [ "best actress oscars"; "best actress academy awards" ] )
    ; "best actor", [ "best actor oscars"; "best actor academy awards" ]
    ; ( "best supporting actress"
      , [ "best supporting actress oscars"
        ; "best supporting actress academy awards"
        ] )
    ; ( "best supporting actor"
      , [ "best supporting actor oscars"
        ; "best supporting actor academy awards"
        ] )
    ; (* charts *)
      "netflix global", [ "top global netflix show"; "global netflix show" ]
    ; "billboard hot 100", [ "billboard 100" ]
    ; (* people who collide in award/championship markets: presence makes
         near-name confusions (Rodri/Pedri) a hard Disagree *)
      "rodri", []
    ; "pedri", []
    ; "vitinha", []
    ; "lamine yamal", []
    ; "jude bellingham", []
    ; "erling haaland", []
    ; "kylian mbappe", []
    ; "ousmane dembele", []
    ; "mohamed salah", []
    ; "harry kane", []
    ; "declan rice", []
    ; "michael olise", []
    ; "lionel messi", []
    ; "rasmus hojgaard", []
    ; "rasmus neergaard petersen", []
    ; "javokhir sindarov", []
    ; "carlos alcaraz", []
    ; "jack draper", []
    ; "victoria mboko", []
    ; "emma navarro", []
    ; "carlos sainz", []
    ; "oscar piastri", []
    ; "lando norris", []
    ; "lewis hamilton", []
    ; "charles leclerc", []
    ; "esteban ocon", []
    ; "isack hadjar", []
    ; "donald trump", []
    ; "yulia navalnaya", []
    ]
  ;;

  (* Table keys are normalized at build time so the entries above can be
     written naturally ("Ballon d'Or" spellings, apostrophes and all). *)
  let canonical_of_spelling =
    List.concat_map aliases ~f:(fun (canonical, spellings) ->
      (normalize canonical, canonical)
      :: List.map spellings ~f:(fun spelling ->
        normalize spelling, canonical))
    |> String.Map.of_alist_reduce ~f:(fun first (_ : string) -> first)
  ;;

  let of_raw raw =
    let normalized = normalize raw in
    match Map.find canonical_of_spelling normalized with
    | Some canonical -> { key = canonical; resolved = true }
    | None -> { key = normalized; resolved = false }
  ;;

  (* Identical strings agree regardless of resolution — two venues both
     writing "guabira" mean the same thing even if we've never heard of it.
     Confident disagreement needs both sides in the table. *)
  let cmp a b : Field_cmp.t =
    if String.equal a.key b.key
    then Agree
    else if a.resolved && b.resolved
    then Disagree
    else Unknown
  ;;
end

module Bound = struct
  type t =
    | Exactly of float
    | At_least of float
    | At_most of float
    | Greater_than of float
    | Less_than of float
    | Between of float * float
  [@@deriving sexp_of, compare, equal]

  let eps = 1e-9

  let to_interval = function
    | Exactly x -> x, x
    | At_least x -> x, Float.infinity
    | Greater_than x -> x +. eps, Float.infinity
    | At_most x -> Float.neg_infinity, x
    | Less_than x -> Float.neg_infinity, x -. eps
    | Between (a, b) -> Float.min a b, Float.max a b
  ;;

  let overlaps a b =
    let alo, ahi = to_interval a
    and blo, bhi = to_interval b in
    Float.( <= ) (Float.max alo blo) (Float.min ahi bhi)
  ;;

  let equivalent a b =
    let alo, ahi = to_interval a
    and blo, bhi = to_interval b in
    (* [Float.equal] first: equal infinities must compare equal, and
       [inf -. inf] is nan, which no tolerance check passes. *)
    let ends_close x y = Float.equal x y || Float.(abs (x -. y) < 1e-6) in
    ends_close alo blo && ends_close ahi bhi
  ;;

  (* True iff the two bounds partition the line: disjoint AND exhaustive,
     e.g. [At_least 0.] and [Less_than 0.]. *)
  let complementary a b =
    let alo, ahi = to_interval a
    and blo, bhi = to_interval b in
    let disjoint = Float.(ahi < blo) || Float.(bhi < alo) in
    let covers_line =
      (Float.is_negative alo
       && Float.is_inf alo
       && Float.is_positive bhi
       && Float.is_inf bhi
       && Float.(ahi >= blo -. (10. *. eps)))
      || (Float.is_negative blo
          && Float.is_inf blo
          && Float.is_positive ahi
          && Float.is_inf ahi
          && Float.(bhi >= alo -. (10. *. eps)))
    in
    disjoint && covers_line
  ;;

  (* [a] admits only numbers [b] also admits: a-YES entails b-YES. *)
  let subset a b =
    let alo, ahi = to_interval a
    and blo, bhi = to_interval b in
    Float.( <= ) blo alo && Float.( <= ) ahi bhi
  ;;
end

module Style = struct
  type t =
    | Terminal
    | Touch
  [@@deriving sexp_of, compare, equal]
end

module Outcome = struct
  type t =
    | Win
    | Place of int
    | Rate_move of Bound.t
    | Threshold of
        { bound : Bound.t
        ; style : Style.t
        }
  [@@deriving sexp_of, compare, equal]
end

module Scope = struct
  type t =
    | Whole_event
    | Leg of
        { kind : string
        ; index : int
        }
  [@@deriving sexp_of, compare, equal]

  let cmp a b : Field_cmp.t =
    if equal a b
    then Agree
    else (
      match a, b with
      (* map 1 vs game 2 is a confident mismatch even across venues'
         nomenclature, because the index differs. *)
      | Leg { index = ia; _ }, Leg { index = ib; _ } ->
        if ia = ib then Unknown else Disagree
      | Whole_event, Leg _ | Leg _, Whole_event -> Disagree
      (* unreachable: equal [Whole_event]s are caught by the guard *)
      | Whole_event, Whole_event -> Agree)
  ;;
end

(* ------------------------------------------------------------------ *)
(* Half-open absolute time windows. *)
(* ------------------------------------------------------------------ *)
module Interval = struct
  type t =
    { lo : Date.t option
    ; hi : Date.t
    }
  [@@deriving sexp_of, compare, equal]

  let of_year year =
    { lo = Some (Date.create_exn ~y:year ~m:Jan ~d:1)
    ; hi = Date.create_exn ~y:(year + 1) ~m:Jan ~d:1
    }
  ;;

  let of_month ~year ~month =
    let lo = Date.create_exn ~y:year ~m:month ~d:1 in
    { lo = Some lo; hi = Date.add_months lo 1 }
  ;;

  (* Venue timestamps for one event straddle midnight, so ends within a day
     still agree. *)
  let ends_close a b = Int.( <= ) (Int.abs (Date.diff a b)) 1

  let cmp a b : Field_cmp.t =
    let lo_close =
      match a.lo, b.lo with
      (* An open start agrees with any start: "before 2027-01-01" is "in
         2026" for a market trading now. *)
      | None, _ | _, None -> true
      | Some a, Some b -> ends_close a b
    in
    if ends_close a.hi b.hi && lo_close then Agree else Disagree
  ;;

  (* [a] inside [b]: a-YES entails b-YES for cumulative events. *)
  let nested a b =
    let lo_within =
      match a.lo, b.lo with
      | _, None -> true
      | None, Some _ -> false
      | Some a, Some b -> Date.( >= ) a b
    in
    lo_within && Date.( <= ) a.hi b.hi
  ;;
end

type t =
  { domain : Domain.t
  ; subject : Entity.t
  ; arena : Entity.t option
  ; outcome : Outcome.t
  ; scope : Scope.t
  ; period : Interval.t option
  }
[@@deriving sexp_of, compare, equal]

module Relation = struct
  type t =
    | Equivalent
    | Complementary
    | Implies
    | Implied_by
    | Disjoint
    | Overlapping
    | Unrelated
  [@@deriving sexp_of, compare, equal]

  let tradeable = function
    | Equivalent | Complementary -> true
    | Implies | Implied_by | Disjoint | Overlapping | Unrelated -> false
  ;;
end

module Verdict = struct
  type t =
    | Decided of
        { relation : Relation.t
        ; deciding : string
        }
    | Abstained of { field : string }
  [@@deriving sexp_of]
end

(* The full relation between two bounds on the same settled quantity. *)
let bound_relation a b : Relation.t =
  if Bound.equivalent a b
  then Equivalent
  else if Bound.complementary a b
  then Complementary
  else if Bound.subset a b
  then Implies
  else if Bound.subset b a
  then Implied_by
  else if Bound.overlaps a b
  then Overlapping
  else Disjoint
;;

(* Outcomes compare into a relation, not a bare Agree/Disagree, because
   bounds grade (equivalent / nested / disjoint). *)
let outcome_relation (a : Outcome.t) (b : Outcome.t) : Relation.t =
  match a, b with
  | Win, Win -> Equivalent
  | Place pa, Place pb -> if pa = pb then Equivalent else Disjoint
  | Win, Place p | Place p, Win -> if p = 1 then Equivalent else Disjoint
  | Rate_move a, Rate_move b -> bound_relation a b
  | Threshold a, Threshold b ->
    (* Touch vs terminal at one strike: different instruments that can both
       settle YES — a confident kill, never an abstention. *)
    if not (Style.equal a.style b.style)
    then Overlapping
    else bound_relation a.bound b.bound
  | Win, (Rate_move _ | Threshold _)
  | (Rate_move _ | Threshold _), Win
  | Place _, (Rate_move _ | Threshold _)
  | (Rate_move _ | Threshold _), Place _
  | Rate_move _, Threshold _
  | Threshold _, Rate_move _ ->
    Disjoint
;;

let entity_cmp_option a b : Field_cmp.t =
  match a, b with
  | Some a, Some b -> Entity.cmp a b
  | None, None -> Agree
  | Some _, None | None, Some _ -> Unknown
;;

(* Field-by-field aggregation, in confidence order:
   1. different domains never speak to each other;
   2. a hard Disagree on subject/arena/scope decides (mutually exclusive
      subjects -> Disjoint, otherwise Unrelated);
   3. a confidently-disjoint outcome decides even when another field is
      Unknown — Winner vs O/U dies without a resolved team name;
   4. period windows: equal agree, nested imply (only when the outcomes
      agree), otherwise the pair is dead;
   5. any remaining Unknown abstains — this pair only, this field only;
   6. all fields agree: the outcome relation is the verdict. *)
let verdict (a : t) (b : t) : Verdict.t =
  if not (Domain.equal a.domain b.domain)
  then Decided { relation = Unrelated; deciding = "domain differs" }
  else (
    let exclusive = Domain.subjects_exclusive a.domain in
    let disagree_relation : Relation.t =
      if exclusive then Disjoint else Unrelated
    in
    let identity_fields =
      [ "subject", Entity.cmp a.subject b.subject
      ; "arena", entity_cmp_option a.arena b.arena
      ; "scope", Scope.cmp a.scope b.scope
      ]
    in
    let identity_disagreement =
      List.find identity_fields ~f:(fun ((_ : string), cmp) ->
        Field_cmp.equal cmp Disagree)
    in
    let outcome = outcome_relation a.outcome b.outcome in
    match identity_disagreement with
    | Some (field, (_ : Field_cmp.t)) ->
      Decided
        { relation = disagree_relation
        ; deciding = [%string "%{field} disagrees"]
        }
    | None ->
      (match outcome with
       | (Disjoint | Complementary) as relation ->
         Decided { relation; deciding = "outcome disagrees" }
       | Equivalent | Implies | Implied_by | Overlapping | Unrelated ->
         let period_cmp : Field_cmp.t =
           match a.period, b.period with
           | Some a, Some b -> Interval.cmp a b
           | None, None | Some _, None | None, Some _ -> Unknown
         in
         (match period_cmp with
          | Disagree ->
            let nested_forward, nested_backward =
              match a.period, b.period with
              | Some pa, Some pb ->
                Interval.nested pa pb, Interval.nested pb pa
              | None, None | Some _, None | None, Some _ -> false, false
            in
            let relation : Relation.t =
              match outcome, nested_forward, nested_backward with
              (* A December hike is a 2026 hike: nested window, matching
                 outcome -> one-sided implication. *)
              | Equivalent, true, false -> Implies
              | Equivalent, false, true -> Implied_by
              | Equivalent, true, true | Equivalent, false, false ->
                if exclusive then Disjoint else Unrelated
              | ( (Implies | Implied_by | Overlapping | Unrelated)
                , true
                , (true | false) )
              | (Implies | Implied_by | Overlapping | Unrelated), false, true
                ->
                Overlapping
              | ( (Implies | Implied_by | Overlapping | Unrelated)
                , false
                , false ) ->
                if exclusive then Disjoint else Unrelated
              | Disjoint, (true | false), (true | false)
              | Complementary, (true | false), (true | false) ->
                (* unreachable: handled as "outcome disagrees" above *)
                Disjoint
            in
            Decided { relation; deciding = "period disagrees" }
          | Unknown -> Abstained { field = "period" }
          | Agree ->
            let unknown =
              List.find identity_fields ~f:(fun ((_ : string), cmp) ->
                Field_cmp.equal cmp Unknown)
            in
            (match unknown with
             | Some (field, (_ : Field_cmp.t)) -> Abstained { field }
             | None ->
               Decided { relation = outcome; deciding = "all fields agree" }))))
;;

(* ------------------------------------------------------------------ *)
(* Blocking *)
(* ------------------------------------------------------------------ *)
module Blocking_key = struct
  module T = struct
    type t =
      { subject : string
      ; period : string
      ; domain : string
      }
    [@@deriving sexp_of, compare]
  end

  include T
  include Comparable.Make_plain (T)
end

let blocking_key claim =
  let period =
    match claim.period with
    | None -> "none"
    (* Month bucket of the window end, so nearby dates still collide. *)
    | Some { lo = _; hi } -> String.prefix (Date.to_string hi) 7
  in
  { Blocking_key.subject = claim.subject.key
  ; period
  ; domain = Domain.to_string claim.domain
  }
;;

(* ------------------------------------------------------------------ *)
(* Title parsing. Conservative on purpose: anything ambiguous fails to
   compile, which costs a string-pipeline referee; a guessed field costs a
   bad trade. *)
(* ------------------------------------------------------------------ *)

(* The trailing parenthetical is the folded outcome/subject subtitle ("Who
   will win the Nobel Peace Prize? (Donald Trump)"). *)
let parenthetical raw =
  let open Option.Let_syntax in
  let%bind start = String.rindex raw '(' in
  let%bind stop = String.rindex raw ')' in
  if stop > start + 1
  then Some (String.sub raw ~pos:(start + 1) ~len:(stop - start - 1))
  else None
;;

let strip_parenthetical raw =
  match String.index raw '(' with
  | Some start -> String.prefix raw start
  | None -> raw
;;

(* Split a leading number off its unit so "25bps" compares like "25 bps".
   "k"/"m" magnitude suffixes stay attached. A trailing '+' stays on the
   number ("25+" marks an at-least bound). *)
let split_number_unit word =
  let numeric =
    String.take_while word ~f:(fun c -> Char.is_digit c || Char.equal c '.')
  in
  if String.is_empty numeric || String.length numeric = String.length word
  then [ word ]
  else (
    let rest = String.drop_prefix word (String.length numeric) in
    let numeric, rest =
      match String.chop_prefix rest ~prefix:"+" with
      | Some rest -> numeric ^ "+", rest
      | None -> numeric, rest
    in
    match rest with
    | "" -> [ numeric ]
    | "k" | "m" -> [ word ]
    | rest -> [ numeric; rest ])
;;

let words_of_title title =
  String.lowercase title
  |> Entity.fold_diacritics
  |> String.substr_replace_all ~pattern:"," ~with_:""
  |> String.substr_replace_all ~pattern:">" ~with_:" more than "
  |> String.substr_replace_all ~pattern:"<" ~with_:" less than "
  |> String.substr_replace_all ~pattern:"#" ~with_:" rank "
  |> String.substr_replace_all ~pattern:"o/u" ~with_:" totalline "
  |> String.map ~f:(fun c ->
    if Char.is_alphanum c || Char.equal c '.' || Char.equal c '+'
    then c
    else ' ')
  |> String.split ~on:' '
  |> List.map ~f:(String.strip ~drop:(Char.equal '.'))
  |> List.filter ~f:(fun word -> not (String.is_empty word))
  |> List.concat_map ~f:split_number_unit
;;

let contains words word = List.mem words word ~equal:String.equal

let contains_any words candidates =
  List.exists candidates ~f:(contains words)
;;

let contains_phrase words phrase =
  let rec loop words =
    match words with
    | [] -> false
    | _ :: rest ->
      List.is_prefix words ~prefix:phrase ~equal:String.equal || loop rest
  in
  loop words
;;

(* "100k" -> 100_000., "1.5m" -> 1_500_000., "0.50" -> 0.5; None for anything
   that isn't purely a number. *)
let number_of_word word =
  let parse text =
    if String.is_empty text
       || (not
             (String.for_all text ~f:(fun c ->
                Char.is_digit c || Char.equal c '.')))
       || not (String.exists text ~f:Char.is_digit)
    then None
    else Option.try_with (fun () -> Float.of_string text)
  in
  let word = String.chop_suffix_if_exists word ~suffix:"+" in
  match String.chop_suffix word ~suffix:"k" with
  | Some prefix -> Option.map (parse prefix) ~f:(fun f -> f *. 1_000.)
  | None ->
    (match String.chop_suffix word ~suffix:"m" with
     | Some prefix -> Option.map (parse prefix) ~f:(fun f -> f *. 1_000_000.)
     | None -> parse word)
;;

(* Bare day-of-month and year integers (and 0, which is never a price): the
   numbers in "June 30" and "in 2026" must not become price levels. *)
let looks_like_date_number level =
  Float.equal level 0.
  || (Float.equal (Float.round_nearest level) level
      && ((Float.( >= ) level 1. && Float.( <= ) level 31.)
          || (Float.( >= ) level 2000. && Float.( <= ) level 2099.)))
;;

let year_of_word word =
  match Option.try_with (fun () -> Int.of_string word) with
  | Some year when year >= 2000 && year <= 2099 -> Some year
  | Some _ | None -> None
;;

let month_of_word word : Month.t option =
  match word with
  | "january" | "jan" -> Some Jan
  | "february" | "feb" -> Some Feb
  | "march" | "mar" -> Some Mar
  | "april" | "apr" -> Some Apr
  | "may" -> Some May
  | "june" | "jun" -> Some Jun
  | "july" | "jul" -> Some Jul
  | "august" | "aug" -> Some Aug
  | "september" | "sep" | "sept" -> Some Sep
  | "october" | "oct" -> Some Oct
  | "november" | "nov" -> Some Nov
  | "december" | "dec" -> Some Dec
  | _ -> None
;;

(* The one month name in the title, if any. "may" is only believed next to
   "meeting" or a year — otherwise it is almost always the modal verb. *)
let title_month words =
  let months =
    List.filter_mapi words ~f:(fun position word ->
      match month_of_word word with
      | None -> None
      | Some Month.May ->
        let neighbor offset =
          List.nth words (position + offset) |> Option.value ~default:""
        in
        let anchored word =
          String.equal word "meeting" || Option.is_some (year_of_word word)
        in
        if anchored (neighbor 1) || anchored (neighbor (-1))
        then Some Month.May
        else None
      | Some month -> Some month)
    |> List.dedup_and_sort ~compare:Month.compare
  in
  match months with [ month ] -> Some month | [] | _ :: _ :: _ -> None
;;

let title_year words =
  match
    List.filter_map words ~f:year_of_word
    |> List.dedup_and_sort ~compare:Int.compare
  with
  | [ year ] -> Some year
  | [] | _ :: _ :: _ -> None
;;

(* "2026-27" season range in the raw title -> [2026-01-01, 2028-01-01):
   loose on purpose, seasons straddle calendar years. *)
let season_range raw =
  let words = String.split_on_chars raw ~on:[ ' '; '('; ')' ] in
  List.find_map words ~f:(fun word ->
    match String.lsplit2 word ~on:'-' with
    | None -> None
    | Some (first, second) ->
      (match
         Option.try_with (fun () ->
           Int.of_string first, Int.of_string second)
       with
       | Some (year, tail) when year >= 2000 && year <= 2099 && tail < 100 ->
         Some
           { Interval.lo = Some (Date.create_exn ~y:year ~m:Jan ~d:1)
           ; hi = Date.create_exn ~y:(2000 + tail + 1) ~m:Jan ~d:1
           }
       | Some _ | None -> None))
;;

(* Full "month day(, year)" date in the word stream. *)
let full_date words ~default_year =
  List.find_mapi words ~f:(fun position word ->
    match month_of_word word with
    | None -> None
    | Some month ->
      let day =
        List.nth words (position + 1)
        |> Option.bind ~f:(fun word ->
          Option.try_with (fun () -> Int.of_string word))
        |> Option.bind ~f:(fun day ->
          if day >= 1 && day <= 31 then Some day else None)
      in
      (match day with
       | None -> None
       | Some day ->
         let year =
           List.nth words (position + 2)
           |> Option.bind ~f:year_of_word
           |> Option.value ~default:default_year
         in
         Option.try_with (fun () -> Date.create_exn ~y:year ~m:month ~d:day)))
;;

(* The explicit time window a title names, if any. Half-open; "by" is
   inclusive of the named day, "before" is not. Month-scoped windows win over
   year-scoped ones: "the September 2026 meeting" is a month, not a year. *)
let title_period words ~close_date =
  let default_year = Date.year close_date in
  let bounded_by prefix =
    contains_phrase words [ prefix ]
    || contains_phrase words [ prefix; "the"; "end"; "of" ]
  in
  match full_date words ~default_year with
  | Some date ->
    let hi = if bounded_by "before" then date else Date.add_days date 1 in
    Some { Interval.lo = None; hi }
  | None ->
    (match title_month words with
     | Some month ->
       let year =
         match title_year words with
         | Some year -> year
         | None ->
           (* A market closes at or shortly after its window, so a title
              month later than the close month means last year. *)
           if Month.to_int month > Month.to_int (Date.month close_date)
           then Date.year close_date - 1
           else Date.year close_date
       in
       Some (Interval.of_month ~year ~month)
     | None ->
       (match title_year words with
        | Some year ->
          if bounded_by "before"
          then
            Some
              { Interval.lo = None
              ; hi = Date.create_exn ~y:year ~m:Jan ~d:1
              }
          else if bounded_by "by"
          then
            Some
              { Interval.lo = None
              ; hi = Date.create_exn ~y:(year + 1) ~m:Jan ~d:1
              }
          else Some (Interval.of_year year)
        | None -> None))
;;

(* Compound outcomes are a parse failure, never a subject: "Stays with Golden
   State or Retires" cannot be one claim. *)
let compound raw =
  let words = words_of_title raw in
  contains words "or" || contains words "any" || contains words "either"
;;

let subject_of_raw raw =
  if compound raw || String.is_empty (Entity.normalize raw)
  then None
  else Some (Entity.of_raw raw)
;;

(* "will <subject> <verb> ..." — [None] when the pre-verb text carries no
   name ("Who will win ...?"). *)
let will_subject before =
  let open Option.Let_syntax in
  let%bind position = String.substr_index before ~pattern:"will " in
  let raw = String.drop_prefix before (position + String.length "will ") in
  subject_of_raw raw
;;

(* ---------- rates ---------- *)

let rate_banks = [ "fed"; "ecb"; "boe"; "boj"; "boc"; "bok"; "rba"; "snb" ]

let bank_of_words words =
  let matched =
    List.filter_map Entity.aliases ~f:(fun (canonical, spellings) ->
      if not (List.mem rate_banks canonical ~equal:String.equal)
      then None
      else if List.exists (canonical :: spellings) ~f:(fun spelling ->
                contains_phrase words (String.split spelling ~on:' '))
      then Some canonical
      else None)
  in
  match matched with [ bank ] -> Some bank | [] | _ :: _ :: _ -> None
;;

let cut_words = [ "cut"; "cuts"; "lower"; "lowers"; "decrease"; "decreases" ]

let hike_words =
  [ "hike"; "hikes"; "raise"; "raises"; "increase"; "increases" ]
;;

let hold_words =
  [ "hold"
  ; "holds"
  ; "unchanged"
  ; "pause"
  ; "pauses"
  ; "maintain"
  ; "maintains"
  ]
;;

let rate_bailout_words = [ "times"; "many"; "meetings" ]
let standard_bps = [ 0.; 25.; 50.; 75.; 100.; 125.; 150. ]

(* "1-25bps" range spellings: two magnitudes joined by a dash. *)
let bps_range raw =
  String.split_on_chars raw ~on:[ ' '; '('; ')' ]
  |> List.find_map ~f:(fun word ->
    let word = String.chop_suffix_if_exists word ~suffix:"bps" in
    match String.lsplit2 word ~on:'-' with
    | None -> None
    | Some (first, second) ->
      (match
         Option.try_with (fun () ->
           Float.of_string first, Float.of_string second)
       with
       | Some (low, high)
         when Float.( > ) high low
              && Float.( <= ) high 200.
              && Float.( >= ) low 1. ->
         Some (low, high)
       | Some _ | None -> None))
;;

let rate_claim raw words ~close_date =
  let open Option.Let_syntax in
  let%bind bank = bank_of_words words in
  if contains_any words rate_bailout_words
     || contains_phrase words [ "less"; "than" ]
  then None
  else (
    let cut = contains_any words cut_words in
    let hike = contains_any words hike_words in
    let hold =
      contains_any words hold_words
      || contains_phrase words [ "no"; "change" ]
    in
    let magnitude =
      if contains_any words [ "bps"; "bp" ]
         || contains_phrase words [ "basis"; "points" ]
      then
        List.filter_map words ~f:number_of_word
        |> List.find ~f:(fun m -> List.mem standard_bps m ~equal:Float.equal)
      else None
    in
    (* The comparator defaults to =. Only an explicit marker widens it:
       ">"/"more than" to strict, "+"/"or more"/"at least" to inclusive. *)
    let strictly_more = contains_phrase words [ "more"; "than" ] in
    let or_more =
      List.exists words ~f:(String.is_suffix ~suffix:"+")
      || contains_phrase words [ "or"; "more" ]
      || contains_phrase words [ "at"; "least" ]
    in
    let%bind delta_bps =
      match bps_range raw, magnitude with
      (* "Cut by 1-25bps": a signed inclusive band. *)
      | Some (low, high), (Some _ | None) ->
        (match cut, hike with
         | true, false -> Some (Bound.Between (-.high, -.low))
         | false, true -> Some (Bound.Between (low, high))
         | true, true | false, false -> None)
      (* "Hike by 0 bps" is a hold: same claim as "no change", disjoint from
         any nonzero move. *)
      | None, Some 0. ->
        if cut || hike || hold then Some (Bound.Exactly 0.) else None
      | None, Some m ->
        (match cut, hike with
         | true, false ->
           Some
             (if strictly_more
              then Bound.Less_than (-.m)
              else if or_more
              then Bound.At_most (-.m)
              else Bound.Exactly (-.m))
         | false, true ->
           Some
             (if strictly_more
              then Bound.Greater_than m
              else if or_more
              then Bound.At_least m
              else Bound.Exactly m)
         | true, true | false, false -> None)
      | None, None ->
        (match cut, hike, hold with
         | true, false, false -> Some (Bound.Less_than 0.)
         | false, true, false -> Some (Bound.Greater_than 0.)
         | false, false, true -> Some (Bound.Exactly 0.)
         | false, false, false -> None
         | true, true, (true | false) | true, false, true | false, true, true
           ->
           None)
    in
    let%map period = title_period words ~close_date in
    { domain = Domain.Rates
    ; subject = Entity.of_raw bank
    ; arena = None
    ; outcome = Outcome.Rate_move delta_bps
    ; scope = Scope.Whole_event
    ; period = Some period
    })
;;

(* ---------- crypto price levels (Statistic) ---------- *)

let touch_words =
  [ "hit"
  ; "hits"
  ; "reach"
  ; "reaches"
  ; "touch"
  ; "touches"
  ; "tap"
  ; "taps"
  ; "cross"
  ; "crosses"
  ]
;;

let dip_words = [ "dip"; "dips"; "drop"; "drops"; "fall"; "falls" ]

let terminal_words =
  [ "close"
  ; "closes"
  ; "settle"
  ; "settles"
  ; "end"
  ; "ends"
  ; "finish"
  ; "finishes"
  ; "stay"
  ; "stays"
  ; "remain"
  ; "remains"
  ]
;;

let above_words =
  [ "above"; "over"; "exceed"; "exceeds"; "surpass"; "surpasses" ]
;;

let below_words = [ "below"; "under" ]
let crypto_assets = [ "btc"; "eth"; "sol"; "doge"; "xrp" ]

let price_claim words ~close_date =
  let open Option.Let_syntax in
  let%bind asset =
    match
      List.filter_map words ~f:(fun word ->
        let entity = Entity.of_raw word in
        if List.mem crypto_assets entity.key ~equal:String.equal
        then Some entity.key
        else None)
      |> List.dedup_and_sort ~compare:String.compare
    with
    | [ asset ] -> Some asset
    | [] | _ :: _ :: _ -> None
  in
  let levels =
    List.filter_map words ~f:number_of_word
    |> List.filter ~f:(fun level -> not (looks_like_date_number level))
  in
  let touch = contains_any words touch_words in
  let dip = contains_any words dip_words in
  let terminal = contains_any words terminal_words in
  let above = contains_any words above_words in
  let below = contains_any words below_words in
  let%bind bound, style =
    match levels with
    | [ level ] ->
      let bound =
        if touch && not below
        then Some (Bound.At_least level)
        else if (touch && below) || dip
        then Some (Bound.At_most level)
        else if above && not below
        then Some (Bound.Greater_than level)
        else if below && not above
        then Some (Bound.Less_than level)
        else None
      in
      let style =
        if touch || dip
        then Some Style.Touch
        else if terminal
        then Some Style.Terminal
        else None
      in
      Option.both bound style
    | [ low; high ] when contains words "between" ->
      let style =
        if touch || dip
        then Some Style.Touch
        else if terminal
        then Some Style.Terminal
        else None
      in
      Option.map style ~f:(fun style -> Bound.Between (low, high), style)
    | [] | [ _; _ ] | _ :: _ :: _ :: _ -> None
  in
  let period =
    match title_period words ~close_date with
    | Some period -> period
    (* No date in the title: the venue's close metadata is the deadline —
       crypto strike markets do keep it accurate. *)
    | None -> { Interval.lo = None; hi = Date.add_days close_date 1 }
  in
  return
    { domain = Domain.Statistic
    ; subject = Entity.of_raw asset
    ; arena = None
    ; outcome = Outcome.Threshold { bound; style }
    ; scope = Scope.Whole_event
    ; period = Some period
    }
;;

(* ---------- championships and awards ---------- *)

let award_arenas =
  [ "ballon d or"
  ; "nobel peace prize"
  ; "best actress"
  ; "best actor"
  ; "best supporting actress"
  ; "best supporting actor"
  ]
;;

let competition_arenas =
  [ "epl"
  ; "ucl"
  ; "f1 drivers"
  ; "f1 constructors"
  ; "world chess championship"
  ; "us open mens"
  ; "us open womens"
  ; "nl championship"
  ; "al championship"
  ; "stanley cup"
  ; "nba finals"
  ; "super bowl"
  ; "world series"
  ; "wyndham championship"
  ; "big brother"
  ]
;;

(* Arena text with years, ordinals, and dangling connectors stripped, so "the
   2026-27 UEFA Champions League Championship", "the Ballon d'Or in 2026",
   and "2 place in Big Brother Season 28" all resolve. *)
let arena_noise = [ "th"; "in"; "at"; "for"; "by"; "their"; "place" ]

let arena_of_raw raw =
  let cleaned =
    Entity.normalize raw
    |> String.split ~on:' '
    |> List.filter ~f:(fun word -> not (String.exists word ~f:Char.is_digit))
    |> List.filter ~f:(fun word ->
      not (List.mem arena_noise word ~equal:String.equal))
    |> String.concat ~sep:" "
  in
  if String.is_empty cleaned then None else Some (Entity.of_raw cleaned)
;;

(* "Will <subject> win|be <arena>?" — the workhorse pattern for both
   championships and awards. The subject may instead live in a trailing
   parenthetical ("Who will win the Ballon d'Or? (Rodri)"). *)
let contest_claim raw words ~close_date:(_ : Date.t) =
  let open Option.Let_syntax in
  let raw_no_paren = strip_parenthetical raw in
  let lowered = String.lowercase raw_no_paren |> Entity.fold_diacritics in
  let%bind before, after =
    List.find_map
      [ " win the "; " win best "; " be the "; " finish in "; " win " ]
      ~f:(fun verb ->
        Option.map
          (String.substr_index lowered ~pattern:verb)
          ~f:(fun position ->
            let after =
              String.drop_prefix lowered (position + String.length verb)
              |> String.filter ~f:(fun c -> not (Char.equal c '?'))
            in
            let after =
              if String.equal verb " win best "
              then "best " ^ after
              else after
            in
            String.prefix lowered position, after))
  in
  let%bind arena = arena_of_raw after in
  let%bind domain =
    if List.mem award_arenas arena.Entity.key ~equal:String.equal
    then Some Domain.Award
    else if List.mem competition_arenas arena.key ~equal:String.equal
    then Some Domain.Championship
    else None
  in
  (* The named subject wins over the parenthetical: "(EPL) Championship"
     parentheticals are arena text, not subjects. *)
  let%bind subject =
    match will_subject before with
    | Some subject -> Some subject
    | None -> Option.bind (parenthetical raw) ~f:subject_of_raw
  in
  let%bind outcome =
    let words_no_paren = words_of_title raw_no_paren in
    if contains_any words_no_paren [ "finish"; "finishes"; "place" ]
    then
      List.filter_map words_no_paren ~f:number_of_word
      |> List.find ~f:(fun n -> Float.( >= ) n 1. && Float.( <= ) n 20.)
      |> Option.map ~f:(fun n -> Outcome.Place (Int.of_float n))
    else Some Outcome.Win
  in
  let period =
    match season_range raw with
    | Some season -> Some season
    | None ->
      (match title_year words with
       | Some year -> Some (Interval.of_year year)
       | None -> None (* period Unknown: abstain rather than guess *))
  in
  return
    { domain
    ; subject
    ; arena = Some arena
    ; outcome
    ; scope = Scope.Whole_event
    ; period
    }
;;

(* ---------- fixtures ("A vs B") ---------- *)

let leg_of_words words =
  List.find_mapi words ~f:(fun position word ->
    let kind =
      match word with
      | "map" | "game" -> Some "game"
      | "set" -> Some "set"
      | "inning" -> Some "inning"
      | "round" -> Some "round"
      | _ -> None
    in
    match kind with
    | None -> None
    | Some kind ->
      List.nth words (position + 1)
      |> Option.bind ~f:(fun next ->
        Option.try_with (fun () -> Int.of_string next))
      |> Option.map ~f:(fun index -> Scope.Leg { kind; index }))
;;

let fixture_structure_words =
  [ "winner"; "match"; "totalline"; "lol"; "dota"; "cs2"; "valorant" ]
;;

let fixture_claim raw words ~close_date =
  let open Option.Let_syntax in
  let stripped = strip_parenthetical raw in
  let lowered = String.lowercase stripped |> Entity.fold_diacritics in
  (* "Will X win map 1 in the A vs. B match?": the fixture text starts after
     "in the"; everything before it is the backed-side clause. *)
  let fixture_text =
    match String.substr_index lowered ~pattern:" in the " with
    | Some position when String.is_substring lowered ~substring:" vs" ->
      String.drop_prefix lowered (position + String.length " in the ")
    | Some _ | None -> lowered
  in
  let%bind position, pattern_length =
    List.find_map [ " vs. "; " vs " ] ~f:(fun pattern ->
      Option.map
        (String.substr_index fixture_text ~pattern)
        ~f:(fun position -> position, String.length pattern))
  in
  let trim raw =
    let raw =
      match String.rsplit2 raw ~on:':' with
      | Some (prefix, suffix) ->
        (* "LoL: Team A" keeps the team; "Team B: O/U 2.5" keeps the team.
           Keep whichever side reads as a name. *)
        if String.exists suffix ~f:Char.is_alpha
           && (not (String.is_substring suffix ~substring:"o/u"))
           && String.length (String.strip suffix) > 3
        then suffix
        else prefix
      | None -> raw
    in
    Entity.normalize raw
    |> String.split ~on:' '
    |> List.filter ~f:(fun word ->
      not (List.mem fixture_structure_words word ~equal:String.equal))
    |> List.filter ~f:(fun word -> not (String.exists word ~f:Char.is_digit))
    |> String.concat ~sep:" "
  in
  let left_team = trim (String.prefix fixture_text position) in
  let right_team =
    trim (String.drop_prefix fixture_text (position + pattern_length))
  in
  let%bind () =
    if String.is_empty left_team || String.is_empty right_team
    then None
    else Some ()
  in
  let fixture_pair =
    let first, second =
      if String.( <= ) left_team right_team
      then left_team, right_team
      else right_team, left_team
    in
    Entity.of_raw [%string "%{first} | %{second}"]
  in
  let total_line =
    List.find_mapi words ~f:(fun position word ->
      if String.equal word "totalline"
      then List.nth words (position + 1) |> Option.bind ~f:number_of_word
      else None)
  in
  let%bind subject, outcome =
    match total_line with
    (* A total backs no team: the fixture itself is the subject. *)
    | Some line ->
      Some
        ( fixture_pair
        , Outcome.Threshold
            { bound = Bound.Greater_than line; style = Style.Terminal } )
    | None ->
      (* A winner market must name its backed side — from the folded subtitle
         or a "Will X win" prefix — or we cannot know which team this market
         is, and false equivalence would be catastrophic. *)
      let backed =
        match parenthetical raw with
        | Some subtitle -> subject_of_raw subtitle
        | None ->
          (* "Will X win ...": the subject ends at the verb. *)
          String.substr_index lowered ~pattern:" win "
          |> Option.bind ~f:(fun verb ->
            will_subject (String.prefix lowered verb))
      in
      Option.map backed ~f:(fun team -> team, Outcome.Win)
  in
  let scope = Option.value (leg_of_words words) ~default:Scope.Whole_event in
  (* A fixture settles on game day; venue close dates are game-accurate here,
     unlike season markets. *)
  let period =
    Some { Interval.lo = None; hi = Date.add_days close_date 1 }
  in
  return
    { domain = Domain.Fixture
    ; subject
    ; arena = Some fixture_pair
    ; outcome
    ; scope
    ; period
    }
;;

(* ---------- media charts ---------- *)

let media_claim raw words ~close_date =
  let open Option.Let_syntax in
  let%bind arena =
    if contains_phrase words [ "netflix"; "show" ]
       || contains_phrase words [ "global"; "netflix" ]
    then Some (Entity.of_raw "netflix global")
    else if contains words "billboard"
    then Some (Entity.of_raw "billboard hot 100")
    else None
  in
  let rank =
    (* "#2" arrives as "rank 2"; bare "Top ..." means #1. *)
    match
      List.find_mapi words ~f:(fun position word ->
        if String.equal word "rank"
        then
          List.nth words (position + 1)
          |> Option.bind ~f:(fun next ->
            Option.try_with (fun () -> Int.of_string next))
        else None)
    with
    | Some rank -> rank
    | None -> 1
  in
  let%bind subject =
    let raw_subject =
      match parenthetical raw with
      | Some subtitle -> Some subtitle
      | None ->
        (* "Will <show> be ..." *)
        let lowered = String.lowercase raw |> Entity.fold_diacritics in
        let%bind.Option start =
          String.substr_index lowered ~pattern:"will "
        in
        let%bind.Option stop = String.substr_index lowered ~pattern:" be " in
        if stop > start + 5
        then
          Some (String.sub lowered ~pos:(start + 5) ~len:(stop - start - 5))
        else None
    in
    Option.bind raw_subject ~f:subject_of_raw
  in
  let period =
    match title_period words ~close_date with
    | Some period -> period
    | None -> { Interval.lo = None; hi = Date.add_days close_date 1 }
  in
  return
    { domain = Domain.Media_rank
    ; subject
    ; arena = Some arena
    ; outcome = Outcome.Place rank
    ; scope = Scope.Whole_event
    ; period = Some period
    }
;;

let of_stub (stub : Market_stub.t) =
  let raw = stub.title in
  let words = words_of_title raw in
  let close_date = Time_ns.to_date stub.close_time ~zone:Timezone.utc in
  List.find_map
    [ rate_claim raw
    ; media_claim raw
    ; fixture_claim raw
    ; contest_claim raw
    ; (fun words ~close_date -> price_claim words ~close_date)
    ]
    ~f:(fun parser -> parser words ~close_date)
;;
