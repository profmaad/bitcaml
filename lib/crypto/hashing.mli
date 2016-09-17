open! Core.Std

val zero_hash : string

type hasher = string -> string

val ripemd160 : hasher
val sha1      : hasher
val sha256    : hasher

val hash160 : hasher
val hash256 : hasher

val message_checksum : hasher

val merkle_tree_hash
  :  hash_f:hasher
  -> string list
  -> string
