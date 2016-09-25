open! Core.Std
open Bitcaml_utils.Std
open Bitcoin_crypto.Std

module Lock_time : sig
  type t =
    | AlwaysLocked
    | BlockLocked     of int32
    | TimestampLocked of Time.t
  [@@deriving bin_io, compare, sexp]


  val of_int32 : int32 -> t
  val to_int32 : t -> int32
end

module Outpoint : sig
  type t [@@deriving bin_io, compare, sexp]

  include Bitstringable        with type t := t
  include Comparable.S_binable with type t := t

  val create
    :  referenced_transaction_hash:Hash_string.t
    -> index:int32
    -> t

  val update
    :  ?referenced_transaction_hash:Hash_string.t
    -> ?index:int32
    -> t
    -> t

  val referenced_transaction_hash : t -> Hash_string.t
  val index                       : t -> int32
end

module Input : sig
  type t [@@deriving bin_io, compare, sexp]

  include Bitstringable with type t := t

  val create
    :  previous_output:Outpoint.t
    -> signature_script:string
    -> sequence_number:int32
    -> t

  val update
    :  ?previous_output:Outpoint.t
    -> ?signature_script:string
    -> ?sequence_number:int32
    -> t
    -> t

  val previous_output  : t -> Outpoint.t
  val signature_script : t -> string
  val sequence_number  : t -> int32
end

module Output : sig
  type t [@@deriving bin_io, compare, sexp]

  include Bitstringable with type t := t

  val create
    :  value:int64
    -> script:string
    -> t

  val update
    :  ?value:int64
    -> ?script:string
    -> t
    -> t

  val value  : t -> int64
  val script : t -> string
end

type t [@@deriving bin_io, compare, sexp]

include Bitstringable with type t := t
include Validation.S  with type t := t

val create
  :  version:int32
  -> inputs:Input.t   Int.Map.t
  -> outputs:Output.t Int.Map.t
  -> lock_time:Lock_time.t
  -> t

val update
  :  ?version:int32
  -> ?inputs:Input.t   Int.Map.t
  -> ?outputs:Output.t Int.Map.t
  -> ?lock_time:Lock_time.t
  -> t
  -> t

val version   : t -> int32
val inputs    : t -> Input.t  Int.Map.t
val outputs   : t -> Output.t Int.Map.t
val lock_time : t -> Lock_time.t

val hash : t -> Hash_string.t

val total_output_value : t -> int64
