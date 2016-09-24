open! Core.Std

type t [@@deriving bin_io, compare, sexp]

val equal : t -> t -> bool

include Stringable.S with type t := t

val to_string_hum : t -> string

val to_bytes : t -> string
val of_bytes : string -> t

val zero : t

type hasher = string -> t

val ripemd160 : hasher
val sha1      : hasher
val sha256    : hasher

val hash160 : hasher
val hash256 : hasher

val message_checksum : hasher

val merkle_tree_hash
  :  t list
  -> t
