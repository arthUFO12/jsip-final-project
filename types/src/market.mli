

type market_type = 
| Single of Single_market.t
| Group of Group_market.t



type t =
{ trade_topic : Trade_topic.t
; market : market_type

}