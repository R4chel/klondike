open Core.Std

type t =
  | Pile_to_pile of Id.t * Id.t
  | Multi_pile_to_pile of Id.t * Id.t * int
  | Refresh_deck

let print t = failwith "TODO"

