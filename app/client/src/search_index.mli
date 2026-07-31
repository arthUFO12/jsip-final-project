(** The market search bar's {!Autocomplete} environment: every backtestable
    market ({!Protocol.Market_card.has_price_history}) is findable by its
    title or its ticker, and a clicked suggestion maps back to its card. *)

open! Core

val env : Protocol.Market_card.t list -> Autocomplete.t

(** The card whose title or slug is exactly the clicked suggestion
    (case-insensitive). *)
val find
  :  Protocol.Market_card.t list
  -> text:string
  -> Protocol.Market_card.t option
