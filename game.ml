open Core.Std
type t = Board.t

let apply_action (board : Board.t) (action : Action.t) =
  match action with
  | Pile_to_pile (p1, p2) ->
    Board.pile_to_pile board p1 p2
  | Multi_pile_to_pile (p1, p2, n) ->
    Board.multi_pile_to_pile board p1 p2 n
;;

let deal_board board =
  List.fold Id.all ~init:board ~f:(fun board pile_id ->
      match pile_id with
      | Pile _i ->
        apply_action board (Action.Pile_to_pile (Id.Deck, pile_id))
      | Hidden_pile i ->
        apply_action board (Action.Multi_pile_to_pile (Id.Deck, pile_id, i))
      | Deck | Discard | Foundation _ -> board
    )
;;

let new_game () =
  let board = deal_board Board.new_board in
  Board.print board
;;


(* let move_to_foundations foundations pile = *)
(*   match Pile.top_card pile with *)
(*   | Some card -> *)
(*     if Foundations.playable foundations card then *)
(*       let pile = Pile.remove_top_card_exn pile in *)
(*       let foundations = Foundations.play_if_playable foundations card in *)
(*       Some (foundations, pile) *)
(*     else *)
(*       None *)
(*   | None -> None *)

(* let try_each_pile_once t = *)
(*   let foundations, piles =  *)
(*     List.fold t.piles ~init:(t.foundations, []) ~f:(fun (acum_foundations, acum_piles) pile -> *)
(*         match move_to_foundations acum_foundations pile with *)
(*         | None -> (acum_foundations, pile :: acum_piles) *)
(*         | Some (foundations, pile) -> (foundations, pile :: acum_piles) *)
(*       ) *)
(*   in *)
(*   { t with piles ; foundations } *)
(* ;; *)

(* let rec play_all_playable t = *)
(*   print_endline "checking if playable...";  *)
(*   let initial_score = Foundations.score t.foundations in *)
(*   let t = try_each_pile_once t in *)
(*   if Foundations.score t.foundations > initial_score then *)
(*     play_all_playable t *)
(*   else *)
(*     t *)
(* ;;  *)

(* let try_add_card_to_piles piles (card : Card.t) = *)
(*     List.fold piles ~init:(false, []) ~f:(fun (played, piles) pile -> *)
(*         if not played && Pile.can_play pile card then *)
(*           (true, (Pile.play pile card) :: piles) *)
(*         else *)
(*           (played, pile :: piles) *)
(*       ) *)
(* ;; *)

(* let print_cards cards = *)
(*   List.map cards ~f:Card.to_string *)
(*   |> String.concat ~sep:" " *)
(*   |> print_endline *)
(*   |> print_newline *)
(* ;; *)

(* let print t turn = *)
(*   print_endline ("======================= Turn " ^ (string_of_int turn) ^ " =========================="); *)
(*   print_endline ("Score: " ^ (string_of_int (Foundations.score t.foundations))); *)
(*   print_endline "Foundations: "; *)
(*   Foundations.print t.foundations; *)
(*   print_newline (); *)
(*   print_endline "Deck: "; *)
(*   Deck.print t.deck; *)
(*   print_endline "Piles"; *)
(*   List.iteri t.piles ~f:(fun i pile -> *)
(*       print_string ("P" ^ (string_of_int i) ^ ": "); *)
(*       Pile.print pile *)
(*     ); *)
(* ;; *)


let () =
  new_game ()
