(* Pretends to trade. Takes an opportunity plus a size and reports what would
   have happened under a chosen fill model: fill at the ask with infinite
   depth, or clamp to real book size and charge fees, or re-read the book
   after a delay to model legging risk. Owns the open positions and the
   running cash balance. The fill model is the dial you turn to show how much
   of the paper edge survives contact with reality. *)
