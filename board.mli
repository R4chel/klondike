open Core.Std

type t = Card.t List.t Id.Map.t

val new_board : t
val print : t -> unit
val pile_to_pile : t -> Id.t -> Id.t -> t
val multi_pile_to_pile : t -> Id.t -> Id.t -> int -> t
val valid : t -> Action.t -> bool
val clean_piles : t -> t

