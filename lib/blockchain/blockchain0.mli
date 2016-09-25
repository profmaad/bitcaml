open! Core.Std
open Bitcoin_crypto.Std
open Bitcoin_protocol.Std

module Node : sig
  type t [@@deriving bin_io, compare, sexp]

  val equal : t -> t -> bool

  val height                : t -> int64
  val cumulative_difficulty : t -> Bignum.Std.Bignum.t
  val hash                  : t -> Hash_string.t
  val header                : t -> Block.Header.t
end

module Orphan : sig
  type t [@@deriving bin_io, compare, sexp]

  val equal : t -> t -> bool

  val hash   : t -> Hash_string.t
  val header : t -> Block.Header.t
end

type t [@@deriving bin_io, compare, sexp]

val empty
  :  genesis_header:Block.Header.t
  -> genesis_hash:Hash_string.t
  -> t

val get_node
  :  t
  -> Hash_string.t
  -> Node.t option

val insert
  :  t
  -> Block.t
  -> t
