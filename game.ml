open Core
type t =
  { board : Board.t
  ; consecutive_discards : int
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

let end_game (game : t) =
  Board.score game.board = 52
  || game.consecutive_discards > 200
;;

let turn (game : t) =
  let action = get_action game.board in
  if not (end_game game) && Board.valid game.board action then
    begin
      let board =
        Board.apply_action game.board action 
        |> Board.clean_piles 
      in
      match action with
      | Action.Refresh_deck | Pile_to_pile (Id.Deck, Id.Discard) ->
        { board ; consecutive_discards = game.consecutive_discards + 1 }
      | _ ->
        { board ; consecutive_discards = 0 }
    end
  else game
;;

let () =
  let num_games = 1000 in
  let sum_score =
    List.fold (List.range 0 num_games) ~init:0 ~f:(fun sum i ->
        Out_channel.output_string stdout (string_of_int i);
        Out_channel.newline stdout;
      (* let board = testing_board i in *)
      let board = new_board () in
      let game = { board ; consecutive_discards = 0 } in
      let game =
      List.fold (List.range 0 5000) ~init:game ~f:(fun game _ ->
         turn game 
        )
      in
      let score = Board.score game.board in
      if Int.equal score 52 then print_endline ("win!");
      (* print_endline ("SCORE " ^ string_of_int score); *)
      sum + score
    )
  in
  let average_score = Int.(//) sum_score num_games in
  print_endline ("AVERAGE SCORE " ^ string_of_float average_score);
;;
