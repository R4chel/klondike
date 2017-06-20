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

let new_board () = deal_board Board.new_board

let get_action t =
  let moves = Board.valid_moves t in
  let r = Random.int (List.length moves) in
  List.nth_exn moves r
;;

let () =
  let board = new_board () in
  let board =
    List.fold (List.range 0 100) ~init:board ~f:(fun board i ->
        print_endline ("======================================== Turn " ^ string_of_int i ^ " ===================");
      Board.print board;
        print_newline ();

      let action = get_action board in
      if Board.valid board action then
        begin
          let board = Board.apply_action board action in
          Board.clean_piles board
        end
      else board
    )
  in
  print_endline "END STATE";
  Board.print board

