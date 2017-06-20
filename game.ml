open Core.Std
type t = Board.t

let deal_board board =
  List.fold Id.all ~init:board ~f:(fun board pile_id ->
      match pile_id with
      | Pile _i ->
        Board.apply_action board (Action.Pile_to_pile (Id.Deck, pile_id))
      | Hidden_pile i ->
        Board.apply_action board (Action.Multi_pile_to_pile (Id.Deck, pile_id, i))
      | Deck | Discard | Foundation _ -> board
    )
;;

let deck i =
  print_endline ("Random seed: " ^ string_of_int i);
  Random.init i;
  List.permute Card.all 
;;

let new_board () =
  deal_board (Board.new_board ())
;;

let testing_board i =
  deal_board (Board.new_board ~deck:(deck i) ())
;;

let get_action t = Player.get_action t

let turn board =
  let action = get_action board in
  if Board.valid board action then
    begin
      let board = Board.apply_action board action in
      Board.clean_piles board
    end
  else board

(* let game = *)
(*   let board = new_board () in *)
(*   let board = *)
(*     List.fold (List.range 0 1000) ~init:board ~f:(fun board i -> *)
(*       (\*   print_endline ("======================================== Turn " ^ string_of_int i ^ " ==================="); *\) *)
(*       (\* Board.print board; *\) *)
(*       (\*   print_newline (); *\) *)
(*         turn board *)
(*     ) *)
(*   in *)
(*   print_endline "END STATE"; *)
(*   Board.print board *)

let () =
  let num_games = 100 in
  let wins =
    List.fold (List.range 0 num_games) ~init:0 ~f:(fun sum i ->
        print_int i; print_newline ();
      (* let board = testing_board i in *)
        let board = new_board () in
      let board =
      List.fold (List.range 0 10000) ~init:board ~f:(fun board _ ->
         turn board 
        )
      in
      if Int.equal (Board.score board) 52 then print_endline ("win!");
      sum + (Board.score board)
    )
  in
  print_endline ("SCORE " ^ string_of_int wins);
  let win_percent = Int.(//) wins num_games in
  print_endline ("SCORE " ^ string_of_float win_percent);
;;


