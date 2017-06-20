open Core.Std

module Value = struct
  type t = int

  let of_int_exn i =
    assert (i >= 1 && i <= 13);
    i
  ;;

  let of_int i =
    if (i >= 1 && i <= 13) then Some i else None
  ;;

  let to_string = function
    | 10 -> "T"
    | 11 -> "J"
    | 12 -> "Q"
    | 13 -> "K"
    | i  -> string_of_int i
  ;;

  let equal = Int.equal
  let equal_int = Int.equal

  let is_prev v1 v2 = v1 = v2 - 1
  let all = List.range 1 14
end

type t =
  { suit : Suit.t
  ; value : Value.t
  }

let to_string t =
  (Suit.to_string t.suit) ^ (Value.to_string t.value)
;;

let all =
  List.fold Suit.all ~init:[] ~f:(fun l suit ->
      List.fold Value.all ~init:l ~f:(fun l value ->
          { suit ; value } :: l ))
;;

let new_deck () =
  let random_state = Random.State.make_self_init () in
  List.permute ~random_state all 
;;

let testing_deck () =
  Random.init 10;
  List.permute all 
