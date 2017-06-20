open Core.Std

type t = Card.t List.t Id.Map.t

let new_board ?deck () =
  let empty = List.fold Id.all ~init:Id.Map.empty ~f:(fun map id ->
     Map.add ~key:id ~data:[] map
    )
  in
  let deck = match deck with Some deck -> deck | None -> Card.new_deck () in
  Map.add empty ~key:Id.Deck ~data:deck

let print t =
  Map.iteri t ~f:(fun ~key ~data ->
      print_string (Id.to_string key);
      print_string ". ";
      List.map data ~f:Card.to_string
      |> String.concat ~sep:" "
      |> print_endline;
    )
;;

let pile_to_pile t id1 id2 =
  let p1 = Map.find_exn t id1 in
  let p2 = Map.find_exn t id2 in
  match p1 with
  | hd :: tl ->
    Map.add t ~key:id1 ~data:tl
    |> Map.add ~key:id2 ~data:(hd::p2)
  | [] ->  t
;;

let multi_pile_to_pile t id1 id2 n =
  let p1 = Map.find_exn t id1 in
  let p2 = Map.find_exn t id2 in
  let cards, p1 = List.split_n p1 n in
  Map.add t ~key:id1 ~data:p1
  |> Map.add ~key:id2 ~data:(cards @ p2)
;;

let clean_piles t =
  List.fold Id.pile_ids ~init:t ~f:(fun t pile_id ->
    match pile_id with
    | Pile i -> 
      let pile = Map.find_exn t (Id.Pile i) in
      let hidden_pile = Map.find_exn t (Id.Hidden_pile i) in
      begin match pile, hidden_pile with
      | [], _ :: _ -> pile_to_pile t (Id.Hidden_pile i) (Id.Pile i)
      | _ -> t
      end
    | _ -> t
  )
;;

let valid_pile_to_pile t (id1 : Id.t) (id2 : Id.t) =
  match id1, id2 with
  | Deck, Discard ->
    let deck = Map.find_exn t Id.Deck in
    not (List.is_empty deck)
  | _ , (Deck | Discard | Hidden_pile _)
  | (Discard | Hidden_pile _ ), _ -> false
  | (Pile _ | Deck | Foundation _), Pile i -> 
    let (p1 : Card.t List.t) = Map.find_exn t id1 in
    let (p2 : Card.t List.t) = Map.find_exn t id2 in
    begin match p1, p2 with
      | [], _ -> false
      | card :: _ , dst :: _ ->
        Card.Value.is_prev card.value dst.value
        && Suit.opposite_colors dst.suit card.suit
      | card :: _ , [] ->
        Card.Value.equal (Card.Value.of_int_exn 13) card.value
    end
  | (Pile _ | Deck | Foundation _), Foundation suit -> 
    let (p1 : Card.t List.t) = Map.find_exn t id1 in
    let (p2 : Card.t List.t) = Map.find_exn t id2 in
    begin match p1 with
      | card :: _ -> 
      Suit.equal suit card.suit &&
      Card.Value.equal_int card.value ((List.length p2) + 1)
      | [] -> false
    end
;;
    
let valid_multi_pile_to_pile t (id1 : Id.t) (id2 : Id.t) n =
  match id1, id2 with
  | Pile i, Pile j ->
    let (p1 : Card.t List.t) = Map.find_exn t id1 in
    let (p2 : Card.t List.t) = Map.find_exn t id2 in
    begin
      match List.nth p1 n, p2 with
      | Some card, dst :: _ ->
        Card.Value.is_prev card.value dst.value
        && Suit.opposite_colors dst.suit card.suit
      | Some card, [] ->
        Card.Value.equal (Card.Value.of_int_exn 13) card.value
      | None , _ -> false
    end
  | _, _ -> false
;;

let valid t (action : Action.t) =
  match action with
  | Pile_to_pile (id1, id2) ->
    valid_pile_to_pile t id1 id2
  | Multi_pile_to_pile (id1, id2, n) ->
    valid_multi_pile_to_pile t id1 id2 n
  | Refresh_deck ->
    let deck = Map.find_exn t Id.Deck in
    List.is_empty deck
;;

let multi_moves t =
  List.map Id.pile_ids ~f:(fun id ->
      let pile = Map.find_exn t id in
      List.map (List.range 2 (List.length pile)) ~f:(fun i -> (id, i))
    )
  |> List.concat_no_order
  |> List.cartesian_product Id.pile_ids
  |> List.map ~f:(fun (id2, (id1, n)) -> Action.Multi_pile_to_pile (id1, id2, n))
;;

let valid_moves t =
  let sources = Id.Deck :: Id.pile_ids in
  let dests = Id.pile_ids @ Id.foundation_ids in
  let all_possible_moves =
    (Id.Deck, Id.Discard) :: List.cartesian_product sources dests
  in
  let actions =
    Action.Refresh_deck
    :: List.map all_possible_moves ~f:(fun (p1, p2) -> Action.Pile_to_pile (p1, p2))
    @ multi_moves t

  in
  List.filter actions ~f:(valid t)
;;

let apply_action t (action : Action.t) =
  match action with
  | Pile_to_pile (p1, p2) ->
    pile_to_pile t p1 p2
  | Multi_pile_to_pile (p1, p2, n) ->
    multi_pile_to_pile t p1 p2 n
  | Refresh_deck ->
    let discard = Map.find_exn t Id.Discard in
    Map.add t ~key:Id.Deck ~data:(List.rev discard)
    |> Map.add ~key:Id.Discard ~data:[]
;;

let score t =
  Map.fold t ~init:0 ~f:(fun ~key ~data sum ->
      match key with
      | Id.Foundation _ -> List.length data + sum
      | _ -> sum
    )
