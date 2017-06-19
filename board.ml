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
   
