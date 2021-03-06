open Core

let action_random_state = Random.State.make_self_init ()

let get_action t ts =
  let moves = Board.valid_non_cyclic_moves t ts in
  (* Out_channel.output_string stdout (" valid moves: " ^ string_of_int (List.length moves)); *)
  let multiply action n = List.map (List.range 0 n) ~f:(fun _ -> action) in
  let weighted_moves =
    List.map moves ~f:(fun (action : Action.t) ->
        match action with
      | Action.Pile_to_pile (Id.Deck, Id.Discard) ->
        multiply action 1
      | Action.Pile_to_pile (_, Id.Foundation _) ->
        multiply action 100
      | _ ->
        multiply action 75
      )
  |> List.concat
  in
  let r = Random.State.int action_random_state (List.length weighted_moves) in
  List.nth_exn weighted_moves r
;;

