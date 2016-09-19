open! Core.Std

type t =
  | MainNetwork
  | TestNet
  | TestNet3
  | Other of int32
[@@deriving compare, sexp]

val of_int32 : int32 -> t
val to_int32 : t -> int32
