(* The store of confirmed pairs and the gate in front of it. Holds proposals
   awaiting human review, records approve/reject decisions, and serves the
   approved set to the bot. This is the boundary: everything upstream is
   guessing about text, everything downstream trusts the pair completely. *)
