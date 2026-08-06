open! Core
open! Async
open! Types

module Listed = struct
  type t =
    { index : int
    ; proposal : Pair_proposal.t
    ; left : Market_stub.t option
    ; right : Market_stub.t option
    }
  [@@deriving sexp_of]

  let title stub market_id =
    match stub with
    | Some (stub : Market_stub.t) ->
      [%string "%{stub.title} [%{stub.venue#Venue}]"]
    | None -> [%string "<no stub for %{market_id#Market_id}>"]
  ;;

  let to_display_string { index; proposal; left; right } =
    let { Pair_proposal.left = left_id
        ; right = right_id
        ; score
        ; explanation
        ; status = _
        }
      =
      proposal
    in
    let line =
      sprintf
        "%2d. [%.2f] %s  <->  %s"
        index
        score
        (title left left_id)
        (title right right_id)
    in
    match explanation with
    | None -> line
    | Some explanation -> [%string "%{line}\n      llm: %{explanation}"]
  ;;
end

let list_by_status status =
  let open Deferred.Or_error.Let_syntax in
  let%bind proposals =
    Database.Database_exec.list_pair_proposals_by_status status
  in
  Deferred.Or_error.List.mapi
    ~how:`Sequential
    proposals
    ~f:(fun index (proposal : Pair_proposal.t) ->
      let%bind left = Database.Database_exec.find_pair_stub proposal.left in
      let%bind right =
        Database.Database_exec.find_pair_stub proposal.right
      in
      return { Listed.index; proposal; left; right })
;;

let list_proposed () = list_by_status Proposed

module Summary = struct
  type t =
    { proposed : int
    ; approved : int
    ; rejected : int
    }
  [@@deriving sexp_of]

  let total { proposed; approved; rejected } = proposed + approved + rejected
end

let summary () =
  let open Deferred.Or_error.Let_syntax in
  (* Counts only, so skip [list_by_status]'s per-entry stub joins. *)
  let count status =
    let%map proposals =
      Database.Database_exec.list_pair_proposals_by_status status
    in
    List.length proposals
  in
  let%bind proposed = count Proposed in
  let%bind approved = count Approved in
  let%bind rejected = count Rejected in
  return { Summary.proposed; approved; rejected }
;;

let decide ~from ~index ~status =
  let open Deferred.Or_error.Let_syntax in
  let%bind listed = list_by_status from in
  match List.nth listed index with
  | None ->
    Deferred.Or_error.error_s
      [%message
        "no such proposal"
          (from : Pair_proposal.Status.t)
          (index : int)
          ~listed:(List.length listed : int)]
  | Some ({ proposal; _ } as entry) ->
    let%bind () =
      Database.Database_exec.set_pair_status
        ~left:proposal.left
        ~right:proposal.right
        status
    in
    return { entry with proposal = { proposal with status } }
;;

module Llm_review_summary = struct
  type t =
    { reviewed : int
    ; approved : int
    ; rejected : int
    ; errored : int
    ; first_error : string option
    }
  [@@deriving sexp_of]
end

let llm_review_proposed ?api_key () =
  let open Deferred.Or_error.Let_syntax in
  let%bind listed = list_proposed () in
  let approved = ref 0 in
  let rejected = ref 0 in
  let errored = ref 0 in
  let first_error = ref None in
  let note_error error =
    incr errored;
    match !first_error with
    | Some (_ : string) -> ()
    | None -> first_error := Some error
  in
  let%bind () =
    Deferred.Or_error.List.iter
      ~how:`Sequential
      listed
      ~f:(fun { Listed.index = _; proposal; left; right } ->
        match left, right with
        | Some left, Some right ->
          (match%bind.Deferred
             Llm_matcher.adjudicate
               ?api_key
               { Matcher.Candidate.left; right }
           with
           | Ok { is_match; explanation } ->
             let status : Pair_proposal.Status.t =
               match is_match with true -> Approved | false -> Rejected
             in
             (match is_match with
              | true -> incr approved
              | false -> incr rejected);
             Database.Database_exec.set_pair_verdict
               ~left:proposal.left
               ~right:proposal.right
               status
               (Some explanation)
           | Error error ->
             (* One bad call (rate limit, hiccup) must not stop the batch; a
                bad key just errors every pair and the count says so. *)
             note_error (Error.to_string_hum error);
             return ())
        | None, Some (_ : Market_stub.t)
        | Some (_ : Market_stub.t), None
        | None, None ->
          note_error
            "pair skipped: a leg has no stored stub to show the model";
          return ())
  in
  return
    { Llm_review_summary.reviewed = List.length listed
    ; approved = !approved
    ; rejected = !rejected
    ; errored = !errored
    ; first_error = !first_error
    }
;;

module Auto_review_summary = struct
  type t =
    { reviewed : int
    ; approved : int
    ; left_proposed : int
    }
  [@@deriving sexp_of]
end

let auto_review_proposed ~threshold =
  let open Deferred.Or_error.Let_syntax in
  let%bind listed = list_proposed () in
  let approved = ref 0 in
  let%bind () =
    Deferred.Or_error.List.iter
      ~how:`Sequential
      listed
      ~f:(fun { Listed.index = _; proposal; left = _; right = _ } ->
        match Float.( >= ) proposal.score threshold with
        | false -> return ()
        | true ->
          incr approved;
          (* The explanation is the audit trail: this verdict came from a
             threshold rule, not from anyone reading the titles. *)
          Database.Database_exec.set_pair_verdict
            ~left:proposal.left
            ~right:proposal.right
            Approved
            (Some
               [%string
                 "auto-approved: text score %{proposal.score#Float} >= \
                  threshold %{threshold#Float}"]))
  in
  return
    { Auto_review_summary.reviewed = List.length listed
    ; approved = !approved
    ; left_proposed = List.length listed - !approved
    }
;;
