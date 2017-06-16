open Core.Std
type t

val empty : t
val playable : t -> Card.t -> bool
val print : t -> unit 
val play_if_playable : t -> Card.t -> t 
val score : t -> int

