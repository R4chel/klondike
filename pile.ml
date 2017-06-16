open Core.Std
type t =
  { top    : Card.t list
  ; hidden : Card.t list
  }

let of_cards cards =
  match cards with
  | hd :: tl -> { top = [hd] ; hidden = tl }
  | [] -> { top = [] ; hidden = [] }
    
let print t = 
  List.map t.top ~f:Card.to_string
  |> String.concat ~sep:" "
  |> print_string;
  print_string " ";
  List.map t.hidden ~f:Card.to_string
  |> List.map ~f:(fun s -> "(" ^ s ^ ")")
  |> String.concat ~sep:" "
  |> print_endline
;;

let can_play (t : t) (card : Card.t) =
  match t.top with
  | [] -> Card.Value.equal (Card.Value.of_int_exn 13) card.value
  | hd :: tl ->
    Card.Value.is_prev card.value hd.value
    && Suit.opposite_colors hd.suit card.suit
;;

let top_card t = List.hd t.top
let remove_top_card_exn t =
  match t.top with
  | [] ->
    assert (List.is_empty t.hidden);
    t
  | [ _hd ] -> 
    let new_top, new_hidden = List.split_n t.hidden 1 in
    { top = new_top ; hidden = new_hidden}
  | _hd :: tl -> { t with top = tl }
;;
