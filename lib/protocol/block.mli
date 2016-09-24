open! Core.Std
open Bitcoin_crypto.Std

module Difficulty : sig
  type t [@@deriving bin_io, compare, sexp]

  val create
    :  base:int32
    -> exponent:int32
    -> t

  val base     : t -> int32
  val exponent : t -> int32

  val of_int32 : int32 -> t
  val to_int32 : t -> int32

  val to_float : t -> float
end

module Header : sig
  type t [@@deriving bin_io, compare, sexp]

  include Bitcaml_utils.Std.Bitstringable with type t := t

  val create
    :  version:int32
    -> previous_block_hash:Hash_string.t
    -> merkle_root:Hash_string.t
    -> timestamp:Time.t
    -> difficulty_target:Difficulty.t
    -> nonce:int32
    -> t

  val version             : t -> int32
  val previous_block_hash : t -> Hash_string.t
  val merkle_root         : t -> Hash_string.t
  val timestamp           : t -> Time.t
  val difficulty_target   : t -> Difficulty.t
  val nonce               : t -> int32
end

module Protocol_header : sig
  type t [@@deriving bin_io, compare, sexp]

  include Bitcaml_utils.Std.Bitstringable with type t := t

  val header            : t -> Header.t
  val transaction_count : t -> int64
end

type t [@@deriving bin_io, compare, sexp]

include Bitcaml_utils.Std.Bitstringable with type t := t

val create
  :  header:Header.t
  -> transactions:Transaction.t list
  -> t

val header       : t -> Header.t
val transactions : t -> Transaction.t list

val hash : t -> Hash_string.t
