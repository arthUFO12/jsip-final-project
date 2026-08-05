open! Core
open! Async
open! Types

(** Real order placement on Kalshi. This is the only module in the codebase
    that sends money anywhere: it signs each request with the account's RSA
    key (Kalshi's API-key scheme — RSA-PSS over [timestamp ^ verb ^ path])
    and POSTs a limit order to the portfolio orders endpoint.

    Nothing here decides {e whether} to trade — that stays in strategies and
    in {!Executor}, which is the only intended caller. Polymarket has no
    module like this yet: its CLOB wants an EIP-712 wallet signature, which
    we do not support, so {!Executor} rejects Polymarket live orders.

    The returned {!Fill.t} reports what the venue said synchronously: the
    taker fill count from the order response, priced at the order's limit
    with the fee estimated by {!Fees.taker_fee}. A resting remainder is not
    tracked; reconciling it via the fills endpoint is future work. *)

(** The production venue. Every credential defaults here; the demo host must
    be chosen explicitly. *)
val live_host : string

(** Kalshi's demo environment — same API, pretend money. Where all
    development and integration testing runs. *)
val demo_host : string

module Credentials : sig
  type t

  (** [create ?host ~key_id ~private_key_pem ()] decodes the PEM (Kalshi
      issues an RSA key in PKCS#8 form) or explains why it will not sign.
      [host] defaults to {!live_host}; pass {!demo_host} for the demo
      environment (demo keys only work there, and vice versa). *)
  val create
    :  ?host:string
    -> key_id:string
    -> private_key_pem:string
    -> unit
    -> t Or_error.t

  (** Which venue these credentials will talk to. *)
  val host : t -> string

  (*_ The env var names [load_from_env] reads, exposed for error messages and
      docs. *)
  val key_id_env : string
  val private_key_file_env : string

  (** Reads [KALSHI_API_KEY_ID] (the key's UUID) and
      [KALSHI_PRIVATE_KEY_FILE] (path to the PEM). Errors name whichever
      variable is missing, so going live fails loudly at startup rather than
      on the first order. The ssh posture applies: a key file readable by
      group or others is refused outright (fix: [chmod 600]). [host] as in
      {!create}. *)
  val load_from_env : ?host:string -> unit -> t Deferred.Or_error.t
end

(** [place_order ?client_order_id credentials order] signs and sends [order].
    Errors before any network traffic if the order is not routable: a
    Polymarket market, a sub-cent limit, a non-positive size.

    [client_order_id] is the idempotency handle: Kalshi rejects a second
    order carrying an id it has already seen, so a caller that retries a
    timed-out placement MUST pass the same id again — that is the whole point
    of supplying it. When omitted, a fresh unique id is minted, which is only
    safe for fire-once callers. *)
val place_order
  :  ?client_order_id:string
  -> Credentials.t
  -> Order.t
  -> Fill.t Deferred.Or_error.t

(** [order_status credentials ~order_id] asks the venue where the order
    stands; returns Kalshi's status string verbatim (e.g. ["resting"],
    ["executed"], ["canceled"]). [order_id] is the venue's id from
    {!Fill.venue_order_id}, not the client order id. *)
val order_status
  :  Credentials.t
  -> order_id:string
  -> string Deferred.Or_error.t

(** [cancel_order credentials ~order_id] cancels the order's resting
    remainder — contracts already filled stay filled — and returns how many
    contracts the cancel removed from the book. *)
val cancel_order
  :  Credentials.t
  -> order_id:string
  -> int Deferred.Or_error.t

(** The account's available cash on this host, in dollars. Demo credentials
    report demo dollars; the number is only as fresh as the venue's read
    side. *)
val balance : Credentials.t -> float Deferred.Or_error.t

module Position : sig
  type t =
    { ticker : string
    ; position : int (** Signed contracts: positive long YES, negative NO. *)
    ; exposure_dollars : float (** The venue's marked exposure. *)
    }
  [@@deriving sexp_of]
end

(** Open (non-zero) per-market positions on this host. *)
val positions : Credentials.t -> Position.t list Deferred.Or_error.t

(** The pure pieces, exposed so tests can exercise signing and encoding
    without a network or a real account. Production code should only use
    {!place_order}. *)
module For_testing : sig
  (** The exact string Kalshi expects under the signature:
      [timestamp_ms ^ verb ^ path], e.g.
      ["1700000000000POST/trade-api/v2/portfolio/orders"]. *)
  val signing_input
    :  timestamp_ms:int
    -> verb:string
    -> path:string
    -> string

  (** Base64 RSA-PSS-SHA256 signature of {!signing_input} — the value of the
      [KALSHI-ACCESS-SIGNATURE] header. Salted, so not deterministic; check
      it with {!verify}. *)
  val sign
    :  credentials:Credentials.t
    -> timestamp_ms:int
    -> verb:string
    -> path:string
    -> string

  (** Verifies a {!sign} result against the public half of the same key —
      what Kalshi's server does with the header. *)
  val verify
    :  credentials:Credentials.t
    -> signature:string
    -> timestamp_ms:int
    -> verb:string
    -> path:string
    -> bool

  (** The JSON body of the order request, or why the order is not routable to
      Kalshi. *)
  val order_body : Order.t -> client_order_id:string -> string Or_error.t

  (** Reads the venue's create-order response into a {!Fill.t}. *)
  val parse_order_response : Order.t -> body:string -> Fill.t Or_error.t

  (** Reads the balance response into dollars. *)
  val parse_balance_response : string -> float Or_error.t

  (** Reads the positions response into open positions. *)
  val parse_positions_response : string -> Position.t list Or_error.t
end
