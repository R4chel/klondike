open Core

type t =
  { board : Board.t
  ; consecutive_discards : int
  ; states : Board.Set.t
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
  || Set.mem game.states game.board
;;

let turn (game : t) =
  let action = get_action game.board in
  if not (end_game game) && Board.valid game.board action then
    begin
      let states = Board.Set.add game.states game.board in
      let board =
        Board.apply_action game.board action 
      in
      match action with
      | Action.Refresh_deck | Pile_to_pile (Id.Deck, Id.Discard) ->
        { board
        ; consecutive_discards = game.consecutive_discards + 1
        ; states
        }
      | _ ->
        { board
        ; consecutive_discards = 0
        ; states
        }
    end
  else game
;;

let rec play_game counter game =
  if end_game game then
    begin
        Out_channel.output_string stdout ("Number of turns: " ^ string_of_int counter);
        Out_channel.newline stdout;
        Out_channel.output_string stdout ("Score: " ^ string_of_int (Board.score game.board));
        Out_channel.newline stdout;
    game
  end
  else
    begin
    turn game
    |> play_game (counter + 1)
  end

;;

let () =
  let num_games = 10 in
  let sum_score =
    List.fold (List.range 0 num_games) ~init:0 ~f:(fun sum i ->
        Out_channel.output_string stdout (string_of_int i);
        Out_channel.newline stdout;
      let board = new_board () in
      let game =
        { board
        ; consecutive_discards = 0
        ; states = Board.Set.empty
        }
      in
      let game = play_game 0 game in
      let score = Board.score game.board in
      if Int.equal score 52 then print_endline ("win!");
      (* print_endline ("SCORE " ^ string_of_int score); *)
      sum + score
    )
  in
  let average_score = Int.(//) sum_score num_games in
  print_endline ("AVERAGE SCORE " ^ string_of_float average_score);
;;
