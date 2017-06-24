open Core

type t =
  | Pile_to_pile of Id.t * Id.t
  | Multi_pile_to_pile of Id.t * Id.t * int
  | Refresh_deck
  | End_game

let to_string = function
  | Pile_to_pile (p1, p2) ->
    "Pile_to_pile : " ^ (Id.to_string p1) ^ " -> " ^ (Id.to_string p2)
  | Multi_pile_to_pile (p1, p2, i) ->
    "Multi_pile_to_pile : " ^ string_of_int i ^ " x " ^ (Id.to_string p1) ^ " -> " ^ (Id.to_string p2)
  | Refresh_deck -> "Refresh deck"
  | End_game -> "End game"

let is_end_game t =
  match t with
  | End_game -> true
  | _ -> false
