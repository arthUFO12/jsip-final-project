open! Core 
open! Async 
open Types 



val fetch_l1_market_data : venue:Venue.t ->
closed:bool ->
limit:int ->
L1_market_metadata.t list Or_error.t Async_kernel__Deferred_or_error.t