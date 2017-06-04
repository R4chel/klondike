open Core.Std
type t [@@deriving sexp]

val all : t list
val to_string : t -> string
val to_full_string : t -> string
(* val t_of_sexp : Sexp.t -> t  *)
