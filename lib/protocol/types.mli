open! Core.Std

module type Bitstringable = sig
  type t

  (* val to_bitstring : t -> Bitstring.t *)
  val of_bitstring : Bitstring.t -> t
end

module Magic : sig
  type t =
    | MainNetwork
    | TestNet
    | TestNet3
    | Other of int32
  [@@deriving compare, sexp]

  val of_int32 : int32 -> t
  val to_int32 : t -> int32
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

module Service : sig
  type t =
    | NetworkNode
  [@@deriving compare, enumerate, sexp]

  include Comparable.S with type t := t

  val of_int64 : int64 -> Set.t
  val to_int64 : Set.t -> int64
end

module Network_address : sig
  type t [@@deriving compare, sexp]

  val services : t -> Service.Set.t
  val address  : t -> Host_and_port.t
end

module Transaction : sig
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

    val referenced_transaction_hash : t -> string
    val index                       : t -> int32
  end

  module Input : sig
    type t [@@deriving compare, sexp]

    val previous_output  : t -> Outpoint.t
    val signature_script : t -> string
    val sequence_number  : t -> int32
  end

  module Output : sig
    type t [@@deriving compare, sexp]

    val value  : t -> int64
    val script : t -> string
  end

  type t [@@deriving compare, sexp]

  include Bitstringable with type t := t

  val version   : t -> int32
  val inputs    : t -> Input.t list
  val outputs   : t -> Output.t list
  val lock_time : t -> Lock_time.t
end

module Block : sig
  module Difficulty : sig
    type t [@@deriving compare, sexp]

    val base     : t -> int32
    val exponent : t -> int32

    val of_int32 : int32 -> t
    val to_int32 : t -> int32
  end

  module Header : sig
    type t [@@deriving compare, sexp]

    val version             : t -> int32
    val previous_block_hash : t -> string
    val merkle_root         : t -> string
    val timestamp           : t -> Time.t
    val difficulty_target   : t -> Difficulty.t
    val nonce               : t -> int32
  end

  module Protocol_header : sig
    type t [@@deriving compare, sexp]

    val header            : t -> Header.t
    val transaction_count : t -> int64
  end

  type t [@@deriving compare, sexp]

  include Bitstringable with type t := t

  val header       : t -> Header.t
  val transactions : t -> Transaction.t list
end

module Messages : sig
  module Version : sig
    type t [@@deriving compare, sexp]

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
  end

  module Getblocks : sig
    type t [@@deriving compare, sexp]

    val protocol_version : t -> int32
    val hashes           : t -> string list
    val hash_stop        : t -> string
  end

  module Headers : sig
    type t = Block.Protocol_header.t list [@@deriving compare, sexp]
  end

  module Nonce : sig
    type t [@@deriving compare, sexp]

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

    val message : t -> string
    val code    : t -> Reason.t
    val reason  : t -> string
  end

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

module Message : sig
  type t [@@deriving compare, sexp]

  (* val to_bitstring : t -> Bitstring.t *)
  val of_bitstring
    :  protocol_version:int
    -> Bitstring.t
    -> t

  val network : t -> Magic.t
  val payload : t -> Messages.t
end
