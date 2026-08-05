open! Core

(** One-time seeding of mirage-crypto's RNG from the kernel. There is no
    mirage-crypto-rng-unix in the switch to plumb entropy automatically, so
    every module that needs randomness — {!Kalshi_live}'s PSS salts,
    {!Wallet_store}'s encryption salts and nonces — calls {!ensure} first.
    Idempotent and cheap after the first call. *)
val ensure : unit -> unit
