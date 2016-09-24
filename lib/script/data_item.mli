open! Core.Std

type t = string [@@deriving bin_io, compare, sexp]

include Bitcaml_utils.Std.Bitstringable with type t := t

val length : t -> int

val equal  : t -> t -> bool

val of_string : string -> t
val to_string : t -> string
val to_string_hum : t -> string

val to_int64 : t -> int64
val of_int64 : int64 -> t

val to_bool : t -> bool
val of_bool : bool -> t

val nget_exn : t -> int -> int
