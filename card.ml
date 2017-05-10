open Core.Std
module Suit = struct
  type t =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

  let to_string = function
    | Clubs -> "C"
    | Diamonds -> "D"
    | Hearts -> "H"
    | Spades -> "S"
  ;; 

  let all =
    [ Clubs
    ; Diamonds
    ; Hearts
    ; Spades
    ]
end

module Value : sig
  type t
  val of_int : int -> t 
  val to_string  : t   -> string
    val all : t list

end = struct
  type t = int

  let of_int i =
    assert (i >= 1 && i <= 13);
    i
  ;;

  let to_string = function
    | 10 -> "T"
    | 11 -> "J"
    | 12 -> "Q"
    | 13 -> "K"
    | i  -> string_of_int i
  ;;

  let all = List.range 1 14
end

type t =
  { suit : Suit.t
  ; value : Value.t
  }

let to_string t =
  (Suit.to_string t.suit) ^ (Value.to_string t.value)
;;

let all =
  List.fold Suit.all ~init:[] ~f:(fun l suit ->
      List.fold Value.all ~init:l ~f:(fun l value ->
          { suit ; value } :: l ))
;;

let new_deck () =
  let random_state = Random.State.make_self_init () in
  List.permute ~random_state all 
;;

let () =
  let deck = new_deck () in
  List.iter deck ~f:(fun card -> 
      print_endline (to_string card)
    )
