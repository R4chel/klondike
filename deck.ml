open Core.Std

type t =
  { deck    : Card.t List.t
  ; discard : Card.t List.t }

let discard_top_card t =
  match t.deck with
  | [] ->
    { deck = List.rev t.discard
    ; discard = []
    }
  | hd :: tl ->
    { deck = tl
    ; discard = hd :: t.discard
    }

let print t =
  List.map t.deck ~f:Card.to_string
  |> String.concat ~sep:" "
  |> print_endline;
  List.map t.discard ~f:Card.to_string
  |> List.map ~f:(fun s -> "(" ^ s ^ ")")
  |> String.concat ~sep:" "
  |> print_endline
  |> print_newline
;;

let top_card t = List.hd t.deck

let of_cards deck = { deck ; discard = [] }

let remove_top_card t =
  match t.deck with 
  | [] ->
    { deck = List.rev t.discard
    ; discard = []
    }
  | hd :: tl ->
    { t with  deck = tl}

let empty_discard t =
  match t.deck with
  | [] ->
    { deck = List.rev t.discard
    ; discard = []
    }
  | hd :: tl -> t
