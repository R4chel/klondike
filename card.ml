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
end

module Value : sig
  type t
  val of_int : int -> t 
  val to_string  : t   -> string

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
end

type t =
  { suit : Suit.t
  ; value : Value.t
  }

let to_string t =
  (Suit.to_string t.suit) ^ (Value.to_string t.value)
;;

let () =
  let card = {suit = Suit.Diamonds ; value = Value.of_int 2 } in
  print_endline (to_string card)
