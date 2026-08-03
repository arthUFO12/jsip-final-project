open! Core
open! Async
open! Types

type summary =
  { markets_swept : int
  ; search_hits : int
  ; fresh_candidates : int
  ; proposed : int
  ; auto_rejected : int
  }
[@@deriving sexp_of]

let listing_limit = 200

(* The search query is the market's blocking tags minus pure numbers — the
   venue's search engine falls back to unrelated trending markets when a
   token like "2035" matches nothing, and numeric agreement is enforced later
   by the veto and the LLM anyway. If nothing but numbers remains, keep them
   rather than search for the empty string. *)
let query_of_title title =
  let tags = Matcher.tags title in
  let words =
    Set.filter tags ~f:(fun tag -> not (String.for_all tag ~f:Char.is_digit))
  in
  let words = if Set.is_empty words then tags else words in
  String.concat ~sep:" " (Set.to_list words)
;;

(* Ordering key within the Kalshi listing: 24h contracts traded. An illiquid
   market can't fund an arbitrage, so its twin isn't worth searching for. *)
let sweep_priority (l1 : L1_market_metadata.t) =
  match l1.volume_24h with
  | Some (Contracts contracts) -> Size.to_int contracts
  | Some (Notional notional) -> Price.to_microcents notional
  | None -> 0
;;

let pair_key left_id right_id =
  let left = Market_id.to_string left_id in
  let right = Market_id.to_string right_id in
  if String.( <= ) left right
  then [%string "%{left}|%{right}"]
  else [%string "%{right}|%{left}"]
;;

let candidate_key ({ left; right } : Matcher.Candidate.t) =
  pair_key left.market_id right.market_id
;;

let insert_stub_if_missing (stub : Market_stub.t) =
  let open Deferred.Or_error.Let_syntax in
  match%bind Database.Database_exec.find_market_stub stub.market_id with
  | Some (_ : Market_stub.t) -> return ()
  | None -> Database.Database_exec.insert_market_stub stub
;;

(* Adjudication costs money, so each market only sends its best-scoring
   candidates to the LLM. *)
let max_candidates_per_market = 10

(* Writes one candidate to the store. LLM-rejected pairs are recorded as
   [Rejected] — with the explanation — so no future sweep pays to ask again;
   the store's DO NOTHING makes the whole write idempotent. Only ever writes
   [Proposed] or [Rejected]: approval stays human. *)
let record_candidate
  ({ left; right } as candidate : Matcher.Candidate.t)
  ~known
  ~explanation
  ~llm_rejected
  =
  let open Deferred.Or_error.Let_syntax in
  let%bind () = insert_stub_if_missing left in
  let%bind () = insert_stub_if_missing right in
  let proposal =
    Pair_proposal.create
      ~left:left.market_id
      ~right:right.market_id
      ~score:(Matcher.score candidate)
      ~explanation
  in
  let%bind () = Database.Database_exec.propose_pair proposal in
  let%bind () =
    if llm_rejected
    then
      Database.Database_exec.set_pair_status
        ~left:proposal.left
        ~right:proposal.right
        Rejected
    else return ()
  in
  Hash_set.add known (candidate_key candidate);
  return ()
;;

(* The store-unaware pairs surviving the text funnel, best score first. *)
let fresh_candidates ~matching ~known lefts rights =
  Matcher.find_candidates
    ~threshold:(Config.Matching.threshold matching)
    ~apply_veto:(Config.Matching.apply_veto matching)
    lefts
    rights
  |> List.filter ~f:(fun candidate ->
    not (Hash_set.mem known (candidate_key candidate)))
  |> List.sort
       ~compare:
         (Comparable.reverse
            (Comparable.lift Float.compare ~f:Matcher.score))
;;

(* Adjudicate (in LLM mode) and record each fresh candidate; returns how many
   landed Proposed and how many the LLM rejected. *)
let record_fresh ~matching ~known fresh =
  let open Deferred.Or_error.Let_syntax in
  let proposed = ref 0 in
  let auto_rejected = ref 0 in
  let%bind () =
    Deferred.Or_error.List.iter ~how:`Sequential fresh ~f:(fun candidate ->
      if not (Config.Matching.use_llm matching)
      then (
        incr proposed;
        record_candidate
          candidate
          ~known
          ~explanation:None
          ~llm_rejected:false)
      else (
        match%bind.Deferred Llm_matcher.adjudicate candidate with
        | Ok { is_match; explanation } ->
          if is_match then incr proposed else incr auto_rejected;
          record_candidate
            candidate
            ~known
            ~explanation:(Some explanation)
            ~llm_rejected:(not is_match)
        | Error error ->
          (* Transient failure: leave the pair unrecorded so a later sweep
             retries it. *)
          Core.eprint_s
            [%message
              "adjudication failed; skipping pair"
                (candidate : Matcher.Candidate.t)
                (error : Error.t)];
          return ()))
  in
  return (!proposed, !auto_rejected)
;;

let sweep_one_market ~matching ~known (l1 : L1_market_metadata.t) =
  let open Deferred.Or_error.Let_syntax in
  let%bind hits =
    Market_data.Market_data_gateway.search_polymarket
      ~query:(query_of_title l1.title)
  in
  let hit_stubs =
    List.filter hits ~f:L1_market_metadata.active
    |> List.map ~f:L1_market_metadata.to_market_stub
  in
  let fresh =
    fresh_candidates
      ~matching
      ~known
      [ L1_market_metadata.to_market_stub l1 ]
      hit_stubs
    |> fun sorted -> List.take sorted max_candidates_per_market
  in
  let%bind proposed, auto_rejected = record_fresh ~matching ~known fresh in
  return (List.length hits, List.length fresh, proposed, auto_rejected)
;;

(* Shared by both sweeps: everything the store already knows, as keys. *)
let known_pairs () =
  let open Deferred.Or_error.Let_syntax in
  let%bind already_known =
    Deferred.Or_error.List.concat_map
      ~how:`Sequential
      Pair_proposal.Status.[ Proposed; Approved; Rejected ]
      ~f:Database.Database_exec.list_pair_proposals_by_status
  in
  return
    (Hash_set.of_list
       (module String)
       (List.map
          already_known
          ~f:(fun { left; right; score = _; explanation = _; status = _ } ->
            pair_key left right)))
;;

let sweep_once ~matching ~markets_to_sweep () =
  let open Deferred.Or_error.Let_syntax in
  let%bind kalshi =
    Market_data.Market_data_gateway.fetch_l1_market_data
      ~venue:Kalshi
      ~closed:false
      ~limit:listing_limit
      ()
  in
  let targets =
    List.sort
      kalshi
      ~compare:
        (Comparable.reverse (Comparable.lift Int.compare ~f:sweep_priority))
    |> fun sorted -> List.take sorted markets_to_sweep
  in
  let%bind known = known_pairs () in
  let summary =
    ref
      { markets_swept = 0
      ; search_hits = 0
      ; fresh_candidates = 0
      ; proposed = 0
      ; auto_rejected = 0
      }
  in
  let%bind () =
    Deferred.Or_error.List.iter ~how:`Sequential targets ~f:(fun l1 ->
      match%bind.Deferred sweep_one_market ~matching ~known l1 with
      | Ok (search_hits, fresh_candidates, proposed, auto_rejected) ->
        summary
        := { markets_swept = !summary.markets_swept + 1
           ; search_hits = !summary.search_hits + search_hits
           ; fresh_candidates = !summary.fresh_candidates + fresh_candidates
           ; proposed = !summary.proposed + proposed
           ; auto_rejected = !summary.auto_rejected + auto_rejected
           };
        return ()
      | Error error ->
        (* One dead search must not kill the sweep. *)
        Core.eprint_s
          [%message
            "search failed; skipping market"
              (l1.market_id : Market_id.t)
              (error : Error.t)];
        return ())
  in
  return !summary
;;

let sweep_full ~matching ~max_adjudications () =
  let open Deferred.Or_error.Let_syntax in
  let%bind kalshi =
    Market_data.Market_data_gateway.fetch_all_l1_market_data ~venue:Kalshi
  in
  let%bind poly =
    Market_data.Market_data_gateway.fetch_all_l1_market_data
      ~venue:Polymarket
  in
  let stubs markets =
    List.filter markets ~f:L1_market_metadata.active
    |> List.map ~f:L1_market_metadata.to_market_stub
  in
  let%bind known = known_pairs () in
  let fresh =
    fresh_candidates ~matching ~known (stubs kalshi) (stubs poly)
  in
  let budgeted =
    if Config.Matching.use_llm matching
    then (
      let kept = List.take fresh max_adjudications in
      let dropped = List.length fresh - List.length kept in
      if dropped > 0
      then
        Core.eprintf
          "sweep_full: %d candidates beyond the %d-adjudication budget wait \
           for the next sweep\n"
          dropped
          max_adjudications;
      kept)
    else fresh
  in
  let%bind proposed, auto_rejected =
    record_fresh ~matching ~known budgeted
  in
  return
    { markets_swept = List.length kalshi
    ; search_hits = List.length poly
    ; fresh_candidates = List.length budgeted
    ; proposed
    ; auto_rejected
    }
;;
