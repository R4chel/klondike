open Core

type color = Red | Black

module T = struct
  type t =
    | Clubs
    | Diamonds
    | Hearts
    | Spades
  [@@deriving sexp]
  let hash = Hashtbl.hash
  let compare = compare
end

include T
include Hashable.Make (T)
let to_full_string = function
  | Clubs    -> "Clubs"
  | Diamonds -> "Diamonds"
  | Hearts   -> "Hearts"
  | Spades   -> "Spades"
;;

let to_string = function
  | Clubs    -> ">3"
  | Diamonds -> "<>"
  | Hearts   -> "<3"
  | Spades   -> ">D"
;;

let all =
  [ Clubs
  ; Diamonds
  ; Hearts
  ; Spades
  ]
;;

let color = function
  | Hearts | Diamonds -> Red
  | Clubs | Spades -> Black
;;

let opposite_colors s1 s2 =
  color s1 <> color s2
;;

let equal s1 s2 = phys_equal s1 s2
