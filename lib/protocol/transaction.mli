open! Core.Std

module Lock_time : sig
  type t =
    | AlwaysLocked
    | BlockLocked     of int32
    | TimestampLocked of Time.t
  [@@deriving compare, sexp]


  val of_int32 : int32 -> t
  val to_int32 : t -> int32
end

module Outpoint : sig
  type t [@@deriving compare, sexp]

  include Bitstringable.S with type t := t

  val referenced_transaction_hash : t -> string
  val index                       : t -> int32
end

module Input : sig
  type t [@@deriving compare, sexp]

  include Bitstringable.S with type t := t

  val previous_output  : t -> Outpoint.t
  val signature_script : t -> string
  val sequence_number  : t -> int32
end

module Output : sig
  type t [@@deriving compare, sexp]

  include Bitstringable.S with type t := t

  val value  : t -> int64
  val script : t -> string
end

type t [@@deriving compare, sexp]

include Bitstringable.S with type t := t

val version   : t -> int32
val inputs    : t -> Input.t list
val outputs   : t -> Output.t list
val lock_time : t -> Lock_time.t

val hash : t -> string
