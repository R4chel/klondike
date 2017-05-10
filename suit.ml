open Core.Std
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
