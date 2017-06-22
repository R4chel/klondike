open Core.Std
type t =
  { board : Board.t
  ; past_actions : Action.t List.t
  }

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

let turn (game : t) =
  let action = get_action game.board in
  if Board.valid game.board action then
    begin
      let board =
        Board.apply_action game.board action 
        |> Board.clean_piles 
      in
      { board ; past_actions = action :: game.past_actions }
    end
  else game

let () =
  let num_games = 100 in
  let wins =
    List.fold (List.range 0 num_games) ~init:0 ~f:(fun sum i ->
        print_int i; print_newline ();
      (* let board = testing_board i in *)
      let board = new_board () in
      let game = { board ; past_actions = [] } in
      let game =
      List.fold (List.range 0 5000) ~init:game ~f:(fun game _ ->
         turn game 
        )
      in
      let score = Board.score game.board in
      if Int.equal score 52 then print_endline ("win!");
      print_endline ("SCORE " ^ string_of_int score);
      sum + score
    )
  in
  print_endline ("SCORE " ^ string_of_int wins);
  let win_percent = Int.(//) wins num_games in
  print_endline ("SCORE " ^ string_of_float win_percent);
;;
