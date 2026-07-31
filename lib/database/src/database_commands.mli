(** SQL requests used by {!Database_gateway} to manage the arbiter database. *)

open! Core

(** Creates the [market_stubs] table if it does not already exist. *)
val create_market_stub_table : (unit, unit, [ `Zero ]) Caqti_request.t

val create_config_table : (unit, unit, [ `Zero ]) Caqti_request.t

val find_market_stub
  : ( Types.Market_id.t
      , Types.Market_stub.t
      , [ `One | `Zero ] )
      Caqti_request.t

val insert_market_stub
  : (Types.Market_stub.t, unit, [ `Zero ]) Caqti_request.t

val list_market_stubs_after
  : ( int64 * int
      , Types.Market_stub.t
      , [ `Many | `One | `Zero ] )
      Caqti_request.t

(** Creates the [pair_proposals] table if it does not already exist. *)
val create_pair_proposal_table : (unit, unit, [ `Zero ]) Caqti_request.t

(** Inserts a proposal; a pair that already exists (in any status) is left
    untouched, so re-proposing never resets a human decision. *)
val insert_pair_proposal
  : (Types.Pair_proposal.t, unit, [ `Zero ]) Caqti_request.t

val set_pair_status
  : ( Types.Pair_proposal.Status.t * Types.Market_id.t * Types.Market_id.t
      , unit
      , [ `Zero ] )
      Caqti_request.t

(** Best score first. *)
val list_pair_proposals_by_status
  : ( Types.Pair_proposal.Status.t
      , Types.Pair_proposal.t
      , [ `Many | `One | `Zero ] )
      Caqti_request.t
