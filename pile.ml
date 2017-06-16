open Core.Std
type t = Card.t list

let of_cards cards = cards

let print t =
  List.map t ~f:Card.to_string
  |> String.concat ~sep:" "
  |> print_endline
;;

let can_play (t : t) (card : Card.t) =
  match t with
  | [] -> Card.Value.equal (Card.Value.of_int_exn 13) card.value 
  | hd :: tl ->
    Card.Value.is_prev card.value hd.value
    && Suit.opposite_colors hd.suit card.suit
;;

let top_card = List.hd
let remove_top_card_exn = List.tl_exn
