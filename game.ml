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

let move_to_foundations foundations pile =
  match Pile.top_card pile with
  | Some card ->
    if Foundations.playable foundations card then
      let pile = Pile.remove_top_card_exn pile in
      let foundations = Foundations.play_if_playable foundations card in
      Some (foundations, pile)
    else
      None
  | None -> None

let play_all_playable t =
  let foundations, piles = 
    List.fold t.piles ~init:(t.foundations, []) ~f:(fun (acum_foundations, acum_piles) pile ->
        match move_to_foundations acum_foundations pile with
        | None -> (acum_foundations, pile :: acum_piles)
        | Some (foundations, pile) -> (foundations, pile :: acum_piles)
      )
  in
  { t with piles ; foundations }
;;

let turn (t : t) (action : Action.t) : t =
  match action with
  | Move_to_foundation _pile -> failwith "TODO"
  | Play_all_playable -> play_all_playable t   
;;

let print_cards cards =
  List.map cards ~f:Card.to_string
  |> String.concat ~sep:" "
  |> print_endline
  |> print_newline
;;

let print t turn =
  print_endline ("======================= Turn " ^ (string_of_int turn) ^ " ==========================");
  print_endline ("Score: " ^ (string_of_int (Foundations.score t.foundations)));
  print_endline "Foundations: ";
  Foundations.print t.foundations;
  print_newline ();
  print_endline "Deck: ";
  print_cards t.deck;
  print_endline "Piles";
  List.iteri t.piles ~f:(fun i pile ->
      print_string ("P" ^ (string_of_int i) ^ ": ");
      Pile.print pile
    );
;;

let () =
  let game = new_game () in
  print game 0;
  let game = turn game Action.Play_all_playable in
  print game 1
;;
