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
      let%bind left =
        Database.Database_exec.find_market_stub proposal.left
      in
      let%bind right =
        Database.Database_exec.find_market_stub proposal.right
      in
      return { Listed.index; proposal; left; right })
;;

let list_proposed () = list_by_status Proposed

let decide ~index ~status =
  let open Deferred.Or_error.Let_syntax in
  let%bind listed = list_proposed () in
  match List.nth listed index with
  | None ->
    Deferred.Or_error.error_s
      [%message
        "no such proposal" (index : int) ~listed:(List.length listed : int)]
  | Some ({ proposal; _ } as entry) ->
    let%bind () =
      Database.Database_exec.set_pair_status
        ~left:proposal.left
        ~right:proposal.right
        status
    in
    return { entry with proposal = { proposal with status } }
;;
