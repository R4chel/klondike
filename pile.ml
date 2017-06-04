open Core.Std
type t = Card.t list

let of_cards cards = cards

let print t =
  List.map t ~f:Card.to_string
  |> String.concat ~sep:" "
  |> print_endline
;;
