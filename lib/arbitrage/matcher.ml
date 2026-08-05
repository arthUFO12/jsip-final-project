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

(* Split spellings of one name, folded to the joined form so "Open AI" and
   "OpenAI" share tags and trigrams. *)
let rec join_split_names words =
  match words with
  | "open" :: "ai" :: rest -> "openai" :: join_split_names rest
  | word :: rest -> word :: join_split_names rest
  | [] -> []
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
  |> join_split_names
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
let jaccard_of_sets left right =
  let union = Set.union left right in
  if Set.is_empty union
  then 0.
  else
    Float.of_int (Set.length (Set.inter left right))
    /. Float.of_int (Set.length union)
;;

let jaccard left right = jaccard_of_sets (trigrams left) (trigrams right)

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

(* Numbers only. Close times are deliberately NOT compared: venue close
   metadata is operational, not event identity — Kalshi files sentinel closes
   years out (2099 for "next NATO Secretary General") and the same Big
   Brother market closed 98 days apart across venues, so a close-time veto
   rejects nearly every real cross-venue pair. Date discipline lives in
   {!Claim.classify}, which parses dates from title text. *)
let numbers_veto ~left_numbers ~right_numbers =
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
  else None
;;

let veto ({ left; right } : Candidate.t) =
  numbers_veto
    ~left_numbers:(numbers left.title)
    ~right_numbers:(numbers right.title)
;;

(* Per-title text features, computed once per market instead of once per
   pair. At full-listing scale (70k x 2k markets, millions of blocked pairs)
   recomputing trigram sets pair-by-pair dominates the entire run. *)
module Text_features = struct
  type t =
    { trigrams : String.Set.t
    ; numbers : String.Set.t
    }
end

let text_features_by_id stubs =
  List.map stubs ~f:(fun (stub : Market_stub.t) ->
    ( stub.market_id
    , { Text_features.trigrams = trigrams stub.title
      ; numbers = numbers stub.title
      } ))
  |> Market_id.Map.of_alist_reduce ~f:(fun first (_ : Text_features.t) ->
    first)
;;

(* Each stub's compiled claim, keyed by market id so the two passes below
   share one compilation. Only compiled markets appear. *)
let claims_by_id stubs =
  List.filter_map stubs ~f:(fun (stub : Market_stub.t) ->
    Option.map (Claim.of_stub stub) ~f:(fun claim -> stub.market_id, claim))
  |> Market_id.Map.of_alist_reduce ~f:(fun first (_ : Claim.t) -> first)
;;

(* Pairs whose compiled claims share a blocking key (subject, window month,
   domain). This catches pairs whose titles share no words at all — "FOMC" vs
   "Federal Reserve" — that tag blocking would miss. *)
let claim_block lefts rights ~left_claims ~right_claims =
  let key claims (stub : Market_stub.t) =
    Option.map (Map.find claims stub.market_id) ~f:Claim.blocking_key
  in
  let index =
    List.filter_map rights ~f:(fun right ->
      Option.map (key right_claims right) ~f:(fun blocking ->
        blocking, right))
    |> Claim.Blocking_key.Map.of_alist_multi
  in
  List.concat_map lefts ~f:(fun left ->
    match key left_claims left with
    | None -> []
    | Some blocking ->
      Map.find index blocking
      |> Option.value ~default:[]
      |> List.map ~f:(fun right -> { Candidate.left; right }))
;;

module Comparison = struct
  module Bucket = struct
    type t =
      | Both
      | Claims_only
      | Text_only
      | Conflict of Claim.Relation.t
      | Neither
    [@@deriving sexp_of]
  end

  type t =
    { candidate : Candidate.t
    ; bucket : Bucket.t
    ; score : float
    ; veto : string option
    ; deciding : string
    ; left_claim : Claim.t option
    ; right_claim : Claim.t option
    }
  [@@deriving sexp_of]

  module Coverage = struct
    module Row = struct
      type t =
        { pairs : int
        ; decided : int
        ; abstentions : int String.Map.t
        }
      [@@deriving sexp_of]

      let empty = { pairs = 0; decided = 0; abstentions = String.Map.empty }

      let merge a b =
        { pairs = a.pairs + b.pairs
        ; decided = a.decided + b.decided
        ; abstentions =
            Map.merge_skewed
              a.abstentions
              b.abstentions
              ~combine:(fun ~key:(_ : string) left right -> left + right)
        }
      ;;
    end

    type t = Row.t String.Map.t [@@deriving sexp_of]

    let merge a b =
      Map.merge_skewed a b ~combine:(fun ~key:(_ : string) left right ->
        Row.merge left right)
    ;;
  end

  module Report = struct
    type comparison = t [@@deriving sexp_of]

    (* Millions of blocked pairs are judged, but almost all are [Neither]
       junk collisions: those exist here only as counts and a bounded
       best-scoring sample, never as a materialized list — at full-listing
       scale the list alone is gigabytes. *)
    type t =
      { judged : comparison list (* every non-Neither pair *)
      ; near_misses : comparison list (* best few Neither, score order *)
      ; neither : int
      ; coverage : Coverage.t
      }
    [@@deriving sexp_of]

    let empty =
      { judged = []
      ; near_misses = []
      ; neither = 0
      ; coverage = String.Map.empty
      }
    ;;
  end
end

(* Enough near-misses to eyeball; anything deeper belongs in a rerun with a
   lower threshold, not in memory. *)
let near_miss_limit = 10

let compare_pipelines ~threshold ~apply_veto lefts rights =
  let left_claims = claims_by_id lefts in
  let right_claims = claims_by_id rights in
  let left_features = text_features_by_id lefts in
  let right_features = text_features_by_id rights in
  let claim_of claims (stub : Market_stub.t) =
    Map.find claims stub.market_id
  in
  (* Candidates only ever hold stubs from [lefts]/[rights], so the feature
     lookup cannot miss. *)
  let features_of features (stub : Market_stub.t) =
    Map.find_exn features stub.market_id
  in
  let judge ~tag_blocked ({ Candidate.left; right } as candidate) =
    let left_text = features_of left_features left in
    let right_text = features_of right_features right in
    let score =
      jaccard_of_sets left_text.Text_features.trigrams right_text.trigrams
    in
    let veto_reason =
      numbers_veto
        ~left_numbers:left_text.numbers
        ~right_numbers:right_text.numbers
    in
    (* The string pipeline's verdict: tag-blocked, above threshold, and (when
       the veto applies) numerically consistent. *)
    let text_pass =
      tag_blocked
      && Float.( >= ) score threshold
      && ((not apply_veto) || Option.is_none veto_reason)
    in
    let left_claim = claim_of left_claims left in
    let right_claim = claim_of right_claims right in
    (* [None] verdict: the claims have nothing to say (a side didn't compile,
       or a field abstained) — the string pipeline referees this pair. *)
    let claim_verdict, deciding =
      match left_claim, right_claim with
      | Some left_claim, Some right_claim ->
        (match Claim.verdict left_claim right_claim with
         | Decided { relation; deciding } -> Some relation, deciding
         | Abstained { field } ->
           None, [%string "abstained: %{field} unknown"])
      | None, (Some _ | None) | Some _, None -> None, "unparsed"
    in
    let bucket : Comparison.Bucket.t =
      match claim_verdict, text_pass with
      | Some Equivalent, true -> Both
      | Some Equivalent, false -> Claims_only
      | None, true -> Text_only
      | ( Some
            (( Complementary | Implies | Implied_by | Disjoint | Overlapping
             | Unrelated ) as relation)
        , true ) ->
        Conflict relation
      | None, false
      | ( Some
            ( Complementary | Implies | Implied_by | Disjoint | Overlapping
            | Unrelated )
        , false ) ->
        Neither
    in
    { Comparison.candidate
    ; bucket
    ; score
    ; veto = veto_reason
    ; deciding
    ; left_claim
    ; right_claim
    }
  in
  let domain_of (comparison : Comparison.t) =
    match comparison.left_claim, comparison.right_claim with
    | Some claim, (Some _ | None) | None, Some claim ->
      Claim.Domain.to_string claim.domain
    | None, None -> "(unparsed)"
  in
  let count_coverage coverage comparison =
    Map.update coverage (domain_of comparison) ~f:(fun row ->
      let row = Option.value row ~default:Comparison.Coverage.Row.empty in
      let row = { row with pairs = row.pairs + 1 } in
      if String.is_prefix comparison.deciding ~prefix:"abstained"
      then
        { row with
          abstentions =
            Map.update row.abstentions comparison.deciding ~f:(fun count ->
              Option.value count ~default:0 + 1)
        }
      else if String.equal comparison.deciding "unparsed"
      then row
      else { row with decided = row.decided + 1 })
  in
  (* Bounded, score-descending; almost every insert loses to the current
     minimum and is O(1). *)
  let insert_near_miss near_misses comparison =
    let full = List.length near_misses >= near_miss_limit in
    match List.last near_misses with
    | Some (worst : Comparison.t)
      when full && Float.( <= ) comparison.Comparison.score worst.score ->
      near_misses
    | Some _ | None ->
      List.take
        (List.sort
           (comparison :: near_misses)
           ~compare:
             (Comparable.reverse
                (Comparable.lift Float.compare ~f:(fun (c : Comparison.t) ->
                   c.score))))
        near_miss_limit
  in
  let absorb (report : Comparison.Report.t) comparison =
    let coverage = count_coverage report.coverage comparison in
    match comparison.Comparison.bucket with
    | Both | Claims_only | Text_only | Conflict (_ : Claim.Relation.t) ->
      { report with judged = comparison :: report.judged; coverage }
    | Neither ->
      { report with
        neither = report.neither + 1
      ; near_misses = insert_near_miss report.near_misses comparison
      ; coverage
      }
  in
  (* Stream: judge each blocked pair as [block] produces it, keeping only the
     judged buckets, the bounded near-miss sample, and counts. The [Neither]
     majority — millions of pairs at full-listing scale — is never
     materialized. *)
  let report =
    List.fold
      (block lefts rights)
      ~init:Comparison.Report.empty
      ~f:(fun report candidate ->
        absorb report (judge ~tag_blocked:true candidate))
  in
  (* Claim-blocked extras: the few pairs sharing a blocking key but no title
     tag. Tag-blocked ones already streamed through above. *)
  let tag_blocked_pair ({ left; right } : Candidate.t) =
    categories_compatible left.category right.category
    && not (Set.are_disjoint (tags left.title) (tags right.title))
  in
  let report =
    List.fold
      (claim_block lefts rights ~left_claims ~right_claims)
      ~init:report
      ~f:(fun report candidate ->
        if tag_blocked_pair candidate
        then report
        else absorb report (judge ~tag_blocked:false candidate))
  in
  { report with judged = List.rev report.judged }
;;

let find_candidates ~threshold ~apply_veto lefts rights =
  let report = compare_pipelines ~threshold ~apply_veto lefts rights in
  List.filter_map
    report.judged
    ~f:(fun { Comparison.candidate; bucket; _ } ->
      match bucket with
      (* Claims decide when both sides compile ([Both]/[Claims_only]); the
         string pipeline decides when they don't ([Text_only]). A [Conflict]
         is a pair the string pipeline liked but the claims veto — including
         [Complementary], a real arb (YES + YES) that {!Detect} cannot price
         yet: fed through as equivalent it would double the bet instead of
         hedging it. Surface those once Detect understands relations. *)
      | Both | Claims_only | Text_only -> Some candidate
      | Conflict (_ : Claim.Relation.t) -> None
      (* Unreachable: [judged] holds no [Neither]. *)
      | Neither -> None)
;;

module For_testing = struct
  let normalize = normalize
  let trigrams = trigrams
  let jaccard = jaccard
  let score = score
  let block = block
  let veto = veto
end
