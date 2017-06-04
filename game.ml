open Core.Std
type t =
  { deck        : Card.t List.t
  ; piles       : Pile.t List.t
  ; foundations : Foundations.t
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
  ; foundations = Foundations.empty
  }

let print_cards cards =
  List.map cards ~f:Card.to_string
  |> String.concat ~sep:" "
  |> print_endline
  |> print_newline
;;

let print t =
  print_endline "Deck: ";
  print_cards t.deck;
  print_endline "Foundations: ";
  Foundations.print t.foundations;
  print_newline ();
  print_endline "Piles";
  List.iteri t.piles ~f:(fun i pile ->
      print_string ("P" ^ (string_of_int i) ^ ": ");
      Pile.print pile
    );
;;

let () =
  let game = new_game () in
  print game
;;

