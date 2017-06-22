open Core.Std

type t =
  | Pile_to_pile of Id.t * Id.t
  | Multi_pile_to_pile of Id.t * Id.t * int
  | Refresh_deck

let to_string = function
  | Pile_to_pile (p1, p2) ->
    "Pile_to_pile : " ^ (Id.to_string p1) ^ " -> " ^ (Id.to_string p2)
  | Multi_pile_to_pile (p1, p2, i) ->
    "Multi_pile_to_pile : " ^ string_of_int i ^ " x " ^ (Id.to_string p1) ^ " -> " ^ (Id.to_string p2)
  | Refresh_deck -> "Refresh deck"
