open! Core.Std
open Bitcoin_crypto.Std
open Bitcoin_protocol.Std

type t [@@deriving sexp_of]

val create
  :  ?folder_levels:int
  -> string
  -> t

val store
  :  t
  -> Block.t
  -> unit

val load
  :  t
  -> Hash_string.t
  -> Block.t
