open! Core.Std

type t =
  | NetworkNode
[@@deriving compare, enumerate, sexp]

include Comparable.S with type t := t

val of_int64 : int64 -> Set.t
val to_int64 : Set.t -> int64
