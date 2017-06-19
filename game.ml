open Core.Std
type t =
  { deck        : Deck.t
  ; piles       : Pile.t List.t
  ; foundations : Foundations.t
  ; board       : Board.t list
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
  { deck = Deck.of_cards deck
  ; piles
  ; foundations = Foundations.empty
  ; board = []

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

let try_each_pile_once t =
  let foundations, piles = 
    List.fold t.piles ~init:(t.foundations, []) ~f:(fun (acum_foundations, acum_piles) pile ->
        match move_to_foundations acum_foundations pile with
        | None -> (acum_foundations, pile :: acum_piles)
        | Some (foundations, pile) -> (foundations, pile :: acum_piles)
      )
  in
  { t with piles ; foundations }
;;

let rec play_all_playable t =
  print_endline "checking if playable..."; 
  let initial_score = Foundations.score t.foundations in
  let t = try_each_pile_once t in
  if Foundations.score t.foundations > initial_score then
    play_all_playable t
  else
    t
;; 

let try_add_card_to_piles piles (card : Card.t) =
    List.fold piles ~init:(false, []) ~f:(fun (played, piles) pile ->
        if not played && Pile.can_play pile card then
          (true, (Pile.play pile card) :: piles)
        else
          (played, pile :: piles)
      )
;;

(* let move_between_piles t = *)
(*   List.fold t.piles ~init:t.piles ~f:(fun piles pile1 -> *)
(*       let card = Pile.top_card pile1 in *)
(*       List.fold piles ~init:  *)

(*     ) *)

let try_from_deck t =
  match Deck.top_card t.deck with
  | None ->
    print_endline "empty discard";
   { t with deck = Deck.empty_discard t.deck } 
  | Some card ->
    if Foundations.playable t.foundations card then
      { t with
        deck = Deck.remove_top_card t.deck
      ; foundations = Foundations.play_if_playable t.foundations card
      }
    else
      begin
        let played, piles = try_add_card_to_piles t.piles card in
        if played then
          { t with
            deck = Deck.remove_top_card t.deck
          ; piles
          }

        else
          { t with
            deck = Deck.discard_top_card t.deck 
          }
      end
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
  Deck.print t.deck;
  print_endline "Piles";
  List.iteri t.piles ~f:(fun i pile ->
      print_string ("P" ^ (string_of_int i) ^ ": ");
      Pile.print pile
    );
;;

let play t =
  let t = play_all_playable t in
  let t = try_from_deck t in
  t
;;

let () =
  let game = new_game () in
  print game 0;
  let _game = 
    List.fold (List.range 1 30) ~init:game ~f:(fun game i ->
        let game = play game in
        print game i;
        game
        )
    in
  print_newline ();
;;
