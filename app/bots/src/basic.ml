open! Core
open Types

module Config = struct
  type t =
    { slugs : Slug.t list
    ; order_threshold : Percentage.t
    ; buy_threshold : Percentage.t
    ; yes_threshold : Percentage.t
    ; rng : Splittable_random.t
    }
end

module State = struct
  type t = { cash : Price.t }
end

module Bot_data = struct
  type t = { placeholder : int }
end

type t =
  { cfg : Config.t
  ; st : State.t
  ; data : Bot_data.t
  }
