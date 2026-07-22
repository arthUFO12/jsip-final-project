open! Core 



type t =
| Elections 
| Politics 
| Sports
| Culture
| Crypto
| Commodities
| Climate 
| Economy 
| Mentions 
| Finance 
| Tech
[@@deriving string, hash, compare, equal]