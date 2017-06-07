open Core.Std

module Value : sig
  type t
  val of_int : int -> t option
  val of_int_exn : int -> t
  val to_string  : t   -> string
  val all : t list

end
type t =
  { suit  : Suit.t
  ; value : Value.t
  }

val new_deck : unit -> t list
val to_string : t -> string
