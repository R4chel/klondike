open Core.Std

type t = Card.t List.t Id.Map.t

let new_board =
  let empty = List.fold Id.all ~init:Id.Map.empty ~f:(fun map id ->
     Map.add ~key:id ~data:[] map
    )
  in
  Map.add empty ~key:Id.Deck ~data:(Card.new_deck ())

let print t =
  Map.iteri t ~f:(fun ~key ~data ->
      print_string (Id.to_string key);
      print_string ". ";
      List.map data ~f:Card.to_string
      |> String.concat ~sep:" "
      |> print_endline;
    )


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

let card_can_move_to_dest t (id : Id.t) (card : Card.t) = 
  match id with
  | Hidden_pile _ | Deck -> false
  | Foundation suit ->
  let pile = Map.find_exn t id in
    Suit.equal suit card.suit &&
    Card.Value.equal_int card.value ((List.length pile) + 1)
  | Pile i -> 
  let pile = Map.find_exn t id in
  begin match (pile : Card.t List.t) with
    | hd :: tl -> 
    Card.Value.is_prev card.value hd.value
    && Suit.opposite_colors hd.suit card.suit
  | [] -> Card.Value.equal (Card.Value.of_int_exn 13) card.value
  end
  | Discard -> false   
;;

let valid_pile_to_pile t (id1 : Id.t) (id2 : Id.t) =
  match id1, id2 with
  | Deck, Discard -> true
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
    