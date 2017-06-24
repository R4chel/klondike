open Core

type t = Card.t List.t Id.Map.t [@@deriving compare, sexp]
include Hashable.S with type t := t
include Comparable.S with type t := t

val new_board : ?deck : Card.t List.t -> unit -> t
val print : t -> unit
val valid : t -> Action.t -> bool
val valid_moves : t -> Action.t List.t
val valid_non_cyclic_moves : t -> Set.t -> Action.t List.t
val apply_action : t -> Action.t -> t
val score : t -> int

