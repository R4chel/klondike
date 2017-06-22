open Core

module T = struct
  type t =
      Pile of int
    | Hidden_pile of int
    | Foundation of Suit.t
    | Deck
    | Discard
  [@@deriving sexp, compare]

  let to_string = function
    | Pile i -> "Pile " ^ string_of_int i
    | Hidden_pile i -> "Hidden_pile " ^ string_of_int i
    | Foundation s -> "Foundation " ^ Suit.to_full_string s
    | Deck -> "Deck"
    | Discard -> "Discard"

  let sexp_of_t t = to_string t |> Sexp.of_string

  let t_of_sexp _sexp = Deck
  let compare = compare
end
include T
include Comparable.Make(T)

let pile_ids = List.map (List.range 0 7) ~f:(fun i -> Pile i)
let hidden_pile_ids = List.map (List.range 0 7) ~f:(fun i -> Hidden_pile i)
let foundation_ids = List.map (Suit.all) ~f:(fun s -> Foundation s)
let all = Deck :: Discard :: foundation_ids @ hidden_pile_ids @ pile_ids
