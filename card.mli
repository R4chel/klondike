open Core

module Value : sig
  type t [@@deriving compare]
  val of_int     : int -> t option
  val of_int_exn : int -> t
  val to_string  : t   -> string
  val all        : t list
  val equal      : t -> t -> bool
  val is_prev    : t -> t -> bool
  val equal_int  : t -> int -> bool
end

type t =
  { suit  : Suit.t
  ; value : Value.t
  }
  [@@deriving sexp]

val all : t List.t
val new_deck : unit -> t List.t
val to_string : t -> string
val testing_deck : unit -> t List.t
