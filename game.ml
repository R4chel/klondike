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

let new_board () = deal_board Board.new_board
;;

let get_action () = Action.Pile_to_pile (Id.Deck, Id.Discard)

let () =
  let board = new_board () in
  let board =
    List.fold (List.range 0 5) ~init:board ~f:(fun board _i ->
      Board.print board;
      let action = get_action () in
      if Board.valid board action then
        begin
          let board = apply_action board action in
          Board.clean_piles board
        end
      else board
    )
  in
  Board.print board

