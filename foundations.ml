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
let playable _t _card =
  failwith "TODO"
