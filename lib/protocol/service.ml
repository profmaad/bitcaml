open! Core.Std

module T = struct
  type t =
    | NetworkNode
  [@@deriving compare, enumerate, sexp]
end
include T

include Comparable.Make(T)

let to_bitmask = function
  | NetworkNode -> 0x0000000000000001L
;;

let of_int64 i =
  List.fold all ~init:Set.empty ~f:(fun set t ->
      match Int64.(bit_and i (to_bitmask t) > zero) with
      | false -> set
      | true  -> Set.add set t)
;;

let to_int64 set =
  Set.fold set ~init:Int64.zero ~f:(fun acc t ->
      Int64.bit_or acc (to_bitmask t))
;;
