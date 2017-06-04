open Core.Std
open Sexplib.Std

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
(* include Hashable.Make (T) *)
let to_full_string = function
  | Clubs    -> "Clubs"
  | Diamonds -> "Diamonds"
  | Hearts   -> "Hearts"
  | Spades   -> "Spades"
;;

let to_string = function
  | Clubs    -> "C"
  | Diamonds -> "D"
  | Hearts   -> "H"
  | Spades   -> "S"
;;

let all =
  [ Clubs
  ; Diamonds
  ; Hearts
  ; Spades
  ]
;;
