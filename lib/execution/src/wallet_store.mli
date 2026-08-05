open! Core
open! Async

(** Encrypted at-rest storage for the user's Kalshi trading key — the disk
    half of the web app's "connect your wallet" flow.

    The user pastes their API key id and private-key PEM once, with a
    passphrase; {!save} validates that the PEM actually signs (via
    {!Kalshi_live.Credentials.create}), encrypts it with a key derived from
    the passphrase (PBKDF2-HMAC-SHA256 then AES-256-GCM), and writes one 0600
    file under the user's home directory. Every later session, {!unlock}
    takes the passphrase again and rebuilds in-memory
    {!Kalshi_live.Credentials.t}; the plaintext PEM never touches disk after
    the initial paste, and a wrong passphrase fails the GCM tag check loudly.
    {!forget} deletes the file.

    The ssh posture from {!Kalshi_live.Credentials.load_from_env} applies on
    read too: a wallet file readable by group or others is refused, not
    warned about. *)

module Status : sig
  (** What the server may tell the browser: whether a key is stored and a
      hint to recognize it by — never the key id in full, never the PEM. *)
  type t =
    | Not_connected
    | Connected of
        { key_hint : string (** e.g. ["cc2a…5e86"] *)
        ; production : bool
        (** [true] = the key targets the real-money host; [false] = demo *)
        }
  [@@deriving sexp_of, equal]
end

(** [~/.kalshi/arbiter-wallet] — kept out of the repo tree entirely, next to
    where the plan tells users to keep their raw PEMs. *)
val default_path : unit -> string

(** [save ~path ~key_id ~private_key_pem ~passphrase ~production ()]
    validates the PEM by constructing credentials for the right host, refuses
    passphrases under 8 characters, then encrypts and writes the wallet file
    (0600, directory created if needed). Overwrites a previous wallet —
    connecting a new key replaces the old one. *)
val save
  :  path:string
  -> key_id:string
  -> private_key_pem:string
  -> passphrase:string
  -> production:bool
  -> unit
  -> unit Deferred.Or_error.t

(** What is on disk right now. [Not_connected] covers both "no file" and
    "file unreadable". *)
val status : path:string -> unit -> Status.t Deferred.t

(** [unlock ~path ~passphrase ()] decrypts the stored PEM and rebuilds
    credentials for the host recorded at {!save} time. A wrong passphrase (or
    tampered file) fails the authenticated decryption and is reported as
    exactly that. *)
val unlock
  :  path:string
  -> passphrase:string
  -> unit
  -> Kalshi_live.Credentials.t Deferred.Or_error.t

(** Deletes the wallet file. Succeeds if it was already gone. *)
val forget : path:string -> unit -> unit Deferred.Or_error.t
