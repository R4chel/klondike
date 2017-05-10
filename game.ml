open Core.Std
type t =
  { deck        : Card.t List.t
  ; piles       : Pile.t List.t
  ; foundations : (Suit.t * Pile.t) List.t
  }

let deal deck =   
  List.fold (List.range 1 8) ~init:(deck, []) ~f:(fun (deck, piles) i ->
    assert (List.length deck >= i);
    let pile, deck = List.split_n deck i in
    (deck, (Pile.of_cards pile) :: piles)
  )
;;

let new_game () =
  let deck = Card.new_deck () in
  let deck, piles = deal deck in
  { deck
  ; piles
  ; foundations = []
  }


let () =
  let game = new_game () in
  List.iter game.deck ~f:(fun card -> print_endline (Card.to_string card))
