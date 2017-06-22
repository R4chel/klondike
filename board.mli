open Core.Std

type t = Card.t List.t Id.Map.t

val new_board : ?deck : Card.t List.t -> unit -> t
val print : t -> unit
val valid : t -> Action.t -> bool
val clean_piles : t -> t
val valid_moves : t -> Action.t List.t
val apply_action : t -> Action.t -> t
val score : t -> int

