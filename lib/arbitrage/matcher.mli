open! Core
open Types

(* Answers "are these two markets the same bet?" using text only.

   Three responsibilities: narrow the candidate set using tags so you aren't
   comparing everything to everything; score the surviving candidates by
   string similarity; veto any candidate whose numbers or dates conflict,
   regardless of how similar the text is. Never sees a price, never touches
   the network, never makes a trade decision. Fully testable with
   hand-written examples. *)

val block : Market.t list -> Market.t list -> Candidate.t list
val score : Candidate.t -> float (* trigram Jaccard *)
val veto : Candidate.t -> string option (* numeric/date conflict *)
