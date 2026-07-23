open! Core 


type direction = 
| Up
| Down

type t =
| Change_pct of {
  arg_name : string
; direction : direction
; start_days_ago : int
; end_days_ago : int
; pct : float
}
| Change_absolute of {
  arg_name : string
; direction : direction
; start_days_ago : int
; end_days_ago : int
; amt : float
}


let evaluate (t : t) _b = 
  ignore t;
  true