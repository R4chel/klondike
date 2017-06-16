open Core.Std

type t

val discard_top_card : t -> t 
val of_cards : Card.t list -> t 
val print : t -> unit 
val top_card : t -> Card.t option
val remove_top_card : t -> t 
val empty_discard : t -> t 
