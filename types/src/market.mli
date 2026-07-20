

(* will end up being abstract *)
type t = {
  name : string 
; odds : Odds.t 
; yes_bbo : Bbo.t
; no_bbo : Bbo.t
; yes_multplier : Multipler.t 
; no_multiplier : Multiplier.t
}


val yes_bbo : t -> Side.t -> Bbo.t
val no_bbo : t -> Side.t -> Bbo.t