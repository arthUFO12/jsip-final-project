open! Core
open Async
open Types

val init_database : string -> unit Or_error.t
val create_market_stub_table : unit -> unit Deferred.Or_error.t
val insert_market_stub : Market_stub.t -> unit Deferred.Or_error.t

val find_market_stub
  :  Market_id.t
  -> Market_stub.t option Deferred.Or_error.t

val list_current_market_stubs : int -> Market_stub.t list Deferred.Or_error.t

module For_testing : sig
  val list_current_market_stubs
    :  int
    -> time:Time_ns.t
    -> Market_stub.t list Deferred.Or_error.t
end

(*_ The pair-proposal store: the review gate described in
    [lib/types/src/pairs.mli]. *)

val create_pair_proposal_table : unit -> unit Deferred.Or_error.t

(** Records a proposal. A pair already present — whatever its status — is
    left untouched, so sweeps can re-propose freely without resurrecting
    rejected pairs or resetting approvals. *)
val propose_pair : Pair_proposal.t -> unit Deferred.Or_error.t

(** The human decision. [left]/[right] must be in the canonical order
    {!Pair_proposal.create} produces. *)
val set_pair_status
  :  left:Market_id.t
  -> right:Market_id.t
  -> Pair_proposal.Status.t
  -> unit Deferred.Or_error.t

(** Best score first. The bot reads [Approved]; review reads [Proposed]. *)
val list_pair_proposals_by_status
  :  Pair_proposal.Status.t
  -> Pair_proposal.t list Deferred.Or_error.t
