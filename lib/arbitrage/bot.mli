(* Orchestration and nothing else. At startup: load config, load markets, run
   matching, hand proposals to the gate, load the confirmed set. Then on a
   timer: fetch books for confirmed pairs, ask detect, ask policy, tell sim,
   tell journal. Contains no arbitrage arithmetic and no matching logic of
   its own — if a formula appears here, it escaped from somewhere. *)
