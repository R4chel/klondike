open Core.Std

module T = struct
  type t =
      Pile of int
    | Hidden_pile of int
    (* | Foundation of Suit.t *)
    | Deck
    | Discard
  [@@deriving sexp, compare]

  let to_string = function
    | Pile i -> "Pile " ^ string_of_int i
    | Hidden_pile i -> "Hidden_pile " ^ string_of_int i
    | Deck -> "Deck"
    | Discard -> "Discard"

  let sexp_of_t t = to_string t |> Sexp.of_string
  let t_of_sexp _sexp = Deck


let compare = compare
end
include T
module Id_map = Map.Make(T)
