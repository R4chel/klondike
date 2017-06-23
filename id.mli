open Core

  type t =
      Pile of int
    | Hidden_pile of int
    | Foundation of Suit.t
    | Deck
    | Discard
  [@@deriving sexp, compare]

include Comparable.S with type t := t

val all : t List.t
val pile_ids : t List.t
val hidden_pile_ids : t List.t
val foundation_ids : t List.t
val to_string : t -> string
