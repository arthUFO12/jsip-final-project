open! Core

(** One booked arbitrage opportunity in the "for fun" wallet: when a scan
    finds a tradable edge, it is recorded here at the profit acting on it
    would have locked in — [edge] dollars per contract (fees already inside,
    from {!Detect}) times [size] contracts of depth. The paper column
    pretends every opportunity was taken; [acted] marks the ones the user
    says they actually traded, freezing the row's numbers.

    Persistence lives in the database library; scoring and display go through
    the server's wallet RPCs. *)

type t =
  { pair_key : string (** canonical ["min_id|max_id"] of the two legs *)
  ; summary : string (** e.g. ["YES Kalshi: ...  +  NO Polymarket: ..."] *)
  ; edge : float (** $ profit per contract, fees included *)
  ; size : int (** contracts of depth behind the edge when booked *)
  ; dollars : float (** [edge * size] — the would-have-made bank *)
  ; acted : bool (** the user says they really placed both legs *)
  ; acted_dollars : float (** [dollars] frozen at the moment of acting *)
  }
[@@deriving sexp_of]
