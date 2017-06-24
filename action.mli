open Core

type t =
  | Pile_to_pile of Id.t * Id.t
  | Multi_pile_to_pile of Id.t * Id.t * int
  | Refresh_deck
  | End_game

val to_string : t -> string

val is_end_game : t -> bool
