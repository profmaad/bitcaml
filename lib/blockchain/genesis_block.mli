open! Core.Std
open Bitcoin_crypto.Std
open Bitcoin_protocol.Std

val header : Magic.t -> Block.Header.t
val hash   : Magic.t -> Hash_string.t
