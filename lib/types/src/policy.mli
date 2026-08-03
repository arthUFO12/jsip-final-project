(* Decides what to do about positions you already hold: when convergence is
   close enough to take profit, whether a newly appeared opportunity is worth
   paying the exit spread twice to rotate into, whether you have capital
   free. Pure, so its rules can be swapped and compared. Starting rule should
   be the dumbest one — never rotate — so everything else has a baseline. *)
