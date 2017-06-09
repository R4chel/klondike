open Core.Std
type t [@@deriving sexp, compare]

val all : t list
val to_string : t -> string
val to_full_string : t -> string
val equal : t -> t -> bool
val opposite_colors : t -> t -> bool
