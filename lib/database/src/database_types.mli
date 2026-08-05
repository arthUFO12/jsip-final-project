open! Core
open Types

val market_stub_type : Market_stub.t Caqti_type.t
val market_id_type : Market_id.t Caqti_type.t
val int64_to_time_ns : int64 -> Time_ns.t
val time_ns_to_int64 : Time_ns.t -> int64
val pair_status_type : Pair_proposal.Status.t Caqti_type.t
val pair_proposal_type : Pair_proposal.t Caqti_type.t
val wallet_entry_type : Wallet_entry.t Caqti_type.t
val trade_log_entry_type : Trade_log_entry.t Caqti_type.t
val arb_observation_type : Arb_observation.t Caqti_type.t
