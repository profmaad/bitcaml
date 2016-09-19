open! Core.Std

module Difficulty : sig
  type t [@@deriving compare, sexp]

  val base     : t -> int32
  val exponent : t -> int32

  val of_int32 : int32 -> t
  val to_int32 : t -> int32
end

module Header : sig
  type t [@@deriving compare, sexp]

  include Bitstringable.S with type t := t

  val version             : t -> int32
  val previous_block_hash : t -> string
  val merkle_root         : t -> string
  val timestamp           : t -> Time.t
  val difficulty_target   : t -> Difficulty.t
  val nonce               : t -> int32
end

module Protocol_header : sig
  type t [@@deriving compare, sexp]

  include Bitstringable.S with type t := t

  val header            : t -> Header.t
  val transaction_count : t -> int64
end

type t [@@deriving compare, sexp]

include Bitstringable.S with type t := t

val header       : t -> Header.t
val transactions : t -> Transaction.t list

val hash : t -> string
