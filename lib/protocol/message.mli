open! Core.Std

module Version : sig
  type t [@@deriving compare, sexp]

  include Bitstringable.S with type t := t

  val protocol_version : t -> int32
  val node_services    : t -> Service.Set.t
  val timestamp        : t -> Time.t
  val receiver_address : t -> Network_address.t
  val sender_address   : t -> Network_address.t option
  val random_nonce     : t -> int64 option
  val user_agent       : t -> string option
  val start_height     : t -> int32 option
  val relay            : t -> bool option
end

module Addr : sig
  module Item : sig
    type t [@@deriving compare, sexp]

    val timestamp : t -> Time.t option
    val address   : t -> Network_address.t
  end

  type t = Item.t list [@@deriving compare, sexp]

  val to_bitstring : t -> Bitstring.t
  val of_bitstring
    :  protocol_version:int32
    -> Bitstring.t
    -> (t * Bitstring.t)
end

module Inventory : sig
  module Item : sig
    module Type : sig
      type t =
        | Transaction
        | Block
        | Other of int32
      [@@deriving compare, sexp]

      val of_int32 : int32 -> t
      val to_int32 : t -> int32
    end

    type t [@@deriving compare, sexp]

    val type_ : t -> Type.t
    val hash  : t -> string
  end

  type t = Item.t list [@@deriving compare, sexp]

  include Bitstringable.S with type t := t
end

module Getblocks : sig
  type t [@@deriving compare, sexp]

  include Bitstringable.S with type t := t

  val version   : t -> int32
  val hashes    : t -> string list
  val hash_stop : t -> string
end

module Headers : sig
  type t = Block.Protocol_header.t list [@@deriving compare, sexp]

  include Bitstringable.S with type t := t
end

module Nonce : sig
  type t [@@deriving compare, sexp]

  include Bitstringable.S with type t := t

  val nonce : t -> int64
end

module Reject : sig
  module Reason : sig
    type t =
      | Malformed
      | Invalid
      | Obsolete
      | Duplicate
      | Nonstandard
      | Dust
      | InsufficientFee
      | Checkpoint
      | Other of int
    [@@deriving compare, sexp]

    val of_int : int -> t
    val to_int : t -> int
  end

  type t [@@deriving compare, sexp]

  include Bitstringable.S with type t := t

  val message : t -> string
  val code    : t -> Reason.t
  val reason  : t -> string
end

module Command : sig
  type t =
    | Version
    | VerAck
    | Addr
    | Inv
    | GetData
    | NotFound
    | GetBlocks
    | GetHeaders
    | Tx
    | Block
    | Headers
    | GetAddr
    | MemPool
    | Ping
    | Pong
    | Reject
    | Alert
    | FilterLoad
    | FilterAdd
    | FilterClear
    | MerkleBlock
    | Other of string
  [@@deriving compare, sexp]

  val of_string : string -> t
  val to_string : t -> string
end

module Payload : sig
  type t =
    | Version     of Version.t
    | VerAck
    | Addr        of Addr.t
    | GetAddr
    | Inv         of Inventory.t
    | GetData     of Inventory.t
    | NotFound    of Inventory.t
    | GetBlocks   of Getblocks.t
    | GetHeaders  of Getblocks.t
    | Tx          of Transaction.t
    | Block       of Block.t
    | Headers     of Headers.t
    | MemPool
    | Ping        of Nonce.t
    | Pong        of Nonce.t
    | Reject      of Reject.t
    | Alert       of Bitstring.t
    | FilterLoad  of Bitstring.t
    | FilterAdd   of Bitstring.t
    | FilterClear of Bitstring.t
    | MerkleBlock of Bitstring.t
    | Other       of { command : string; payload : Bitstring.t }
  [@@deriving compare, sexp]

  val to_command : t -> Command.t
end

type t [@@deriving compare, sexp]

val to_bitstring : t -> Bitstring.t
val of_bitstring
  :  protocol_version:int32
  -> Bitstring.t
  -> (t * Bitstring.t)

val network : t -> Magic.t
val payload : t -> Payload.t
