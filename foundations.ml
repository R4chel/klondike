open Core.Std
type t = (Suit.t * Card.t List.t) List.t

let empty =
  List.map Suit.all ~f:(fun suit -> (suit, []))

let print t =
  List.iter t ~f:(fun (suit, cards) ->
      let card =
        match cards with
        | hd :: _tl -> Card.to_string hd
        | [] -> "EMPTY"
      in
      print_endline ((Suit.to_full_string suit) ^ ": " ^ card)
    )
;;

let playable_cards t =
  List.filter_map t ~f:(fun (suit, cards) ->
      let value = (List.length cards) + 1 in
      Option.map (Card.Value.of_int value) ~f:(fun value ->
          { Card.suit ; value }
        ))
;;

let playable (t : t) (card: Card.t) =
  List.exists (playable_cards t) ~f:(fun c ->
      Suit.equal c.suit card.suit
      && Card.Value.equal c.value card.value
    )
;;

let play_if_playable (t : t) (card : Card.t) =
  List.map t ~f:(fun (suit, cards) ->
    if Suit.equal card.suit suit
    && Card.Value.equal_int card.value ((List.length cards) + 1)
    then (suit, card :: cards)
    else (suit, cards)
    )
;;

let score t =
  let count = List.fold t ~init:0 ~f:(fun sum (_suit, l) -> (List.length l) + sum) in
  count * 5
