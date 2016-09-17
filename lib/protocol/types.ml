open! Core.Std
open Bitcaml_utils.Std

module Magic = struct
  type t =
    | Unknown of int32
    | MainNetwork
    | TestNet
    | TestNet3
  [@@deriving compare, sexp]

  let of_int32 = function
    | 0xD9B4BEF9l -> MainNetwork
    | 0xDAB5BFFAl -> TestNet
    | 0x0709110Bl -> TestNet3
    | i           -> Unknown i
  ;;

  let to_int32 = function
    | MainNetwork -> 0xD9B4BEF9l
    | TestNet     -> 0xDAB5BFFAl
    | TestNet3    -> 0x0709110Bl
    | Unknown i   -> i
  ;;
end

module Command = struct
  type t =
    | Unknown of string
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
  [@@deriving compare, sexp]

  let of_string = function
    | "version"     -> Version
    | "verack"      -> VerAck
    | "addr"        -> Addr
    | "inv"         -> Inv
    | "getdata"     -> GetData
    | "notfound"    -> NotFound
    | "getblocks"   -> GetBlocks
    | "getheaders"  -> GetHeaders
    | "tx"          -> Tx
    | "block"       -> Block
    | "headers"     -> Headers
    | "getaddr"     -> GetAddr
    | "mempool"     -> MemPool
    | "ping"        -> Ping
    | "pong"        -> Pong
    | "reject"      -> Reject
    | "alert"       -> Alert
    | "filterload"  -> FilterLoad
    | "filteradd"   -> FilterAdd
    | "filterclear" -> FilterClear
    | "merkleblock" -> MerkleBlock
    | s             -> Unknown s
  ;;

  let to_string = function
    | Version     -> "version"
    | VerAck      -> "verack"
    | Addr        -> "addr"
    | Inv         -> "inv"
    | GetData     -> "getdata"
    | NotFound    -> "notfound"
    | GetBlocks   -> "getblocks"
    | GetHeaders  -> "getheaders"
    | Tx          -> "tx"
    | Block       -> "block"
    | Headers     -> "headers"
    | GetAddr     -> "getaddr"
    | MemPool     -> "mempool"
    | Ping        -> "ping"
    | Pong        -> "pong"
    | Reject      -> "reject"
    | Alert       -> "alert"
    | FilterLoad  -> "filterload"
    | FilterAdd   -> "filteradd"
    | FilterClear -> "filterclear"
    | MerkleBlock -> "merkleblock"
    | Unknown s   -> s
  ;;
end

(* Services defined for the services field *)
module Service = struct
  module T = struct
    type t =
      | NetworkNode
    [@@deriving compare, enumerate, sexp]
  end
  include T

  include Comparable.Make(T)

  let to_bitmask = function
    | NetworkNode -> 0x0000000000000001L
  ;;

  let of_int64 i =
    List.fold all ~init:Set.empty ~f:(fun set t ->
        match Int64.(bit_and i (to_bitmask t) > zero) with
        | false -> set
        | true  -> Set.add set t)
  ;;

  let to_int64 set =
    Set.fold set ~init:Int64.zero ~f:(fun acc t ->
        Int64.bit_or acc (to_bitmask t))
  ;;
end

module Network_address = struct
  type t =
    { services : Service.Set.t
    ; address  : string
    ; port     : int
    } [@@deriving compare, fields, sexp]
end

module Header = struct
  type t =
    { magic          : Magic.t
    ; command        : Command.t
    ; payload_length : int
    ; checksum       : string
    } [@@deriving compare, fields, sexp]
end

module Transaction = struct
  module Lock_time = struct
    type t =
      | AlwaysLocked
      | BlockLocked     of int32
      | TimestampLocked of Time.t
    [@@deriving compare, sexp]


    let of_int32 = function
      | 0x0l                    -> AlwaysLocked
      | i when (i < 500000000l) -> BlockLocked i
      | i                       -> TimestampLocked (Int32.to_float i |> Time.of_epoch)
    ;;

    let to_in32 = function
      | AlwaysLocked              -> 0x0l
      | BlockLocked i             -> i
      | TimestampLocked timestamp ->
        Time.to_epoch timestamp
        |> Int32.of_float
    ;;
  end

  module Outpoint = struct
    type t =
      { referenced_transaction_hash : string
      ; index                       : int32
      } [@@deriving compare, fields, sexp]
  end

  module Input = struct
    type t =
      { previous_output             : Outpoint.t
      ; signature_script            : string
      ; transaction_sequence_number : int32
      } [@@deriving compare, fields, sexp]
  end

  module Output = struct
    type t =
      { value  : int64
      ; script : string
      } [@@deriving compare, fields, sexp]
  end

  type t =
    { version   : int32
    ; inputs    : Input.t list
    ; outputs   : Output.t list
    ; lock_time : Lock_time.t
    } [@@deriving compare, fields, sexp]
end

module Block = struct
  module Difficulty = struct
    type t =
      { base     : int32
      ; exponent : int32
      } [@@deriving compare, fields, sexp]

    let of_int32 i =
      Fields.create
        ~base:(Int32.bit_and i 0x00ffffffl)
        ~exponent:(Int32.shift_right_logical (Int32.bit_and i 0xff000000l) 24)
    ;;

    let to_int32 t =
      let exponent = Int32.shift_left (exponent t) 24 in
      Int32.bit_or exponent (base t)
    ;;
  end

  module Header = struct
    type t =
      { version             : int32
      ; previous_block_hash : string
      ; merkle_root         : string
      ; timestamp           : Time.t
      ; difficulty_target   : Difficulty.t
      ; nonce               : int32
      } [@@deriving compare, fields, sexp]
  end

  module Protocol_header = struct
    type t =
      { basic_header      : Header.t
      ; transaction_count : int64
      } [@@deriving compare, fields, sexp]
  end

  type t =
    { header       : Header.t
    ; transactions : Transaction.t list
    } [@@deriving compare, fields, sexp]
end

module Messages = struct
  module Version = struct
    type t =
      { protocol_version : int
      ; node_services    : Service.Set.t
      ; timestamp        : Time.t
      ; receiver_address : Network_address.t
      ; sender_address   : Network_address.t option
      ; random_nonce     : int64 option
      ; user_agent       : string option
      ; start_height     : int option
      ; relay            : bool option
      } [@@deriving compare, fields, sexp]
  end

  module Addr = struct
    module Item = struct
      type t =
        { timestamp : Time.t option
        ; address   : Network_address.t
        } [@@deriving compare, fields, sexp]
    end

    type t = Item.t list [@@deriving compare, sexp]
  end

  module Inventory = struct
    module Item = struct
      module Type = struct
        type t =
          | Transaction
          | Block
          | Unknown of int32
        [@@deriving compare, sexp]

        let of_int32 = function
          | 0x1l -> Transaction
          | 0x2l -> Block
          | i    -> Unknown i
        ;;

        let to_int32 = function
          | Transaction -> 0x1l
          | Block       -> 0x2l
          | Unknown i   -> i
        ;;
      end

      type t =
        { type_ : Type.t
        ; hash  : string
        } [@@deriving compare, fields, sexp]
    end

    type t = Item.t list [@@deriving compare, sexp]
  end

  module Getblocks = struct
    type t =
      { protocol_version : int32
      ; hashes           : string list
      ; hash_stop        : string
      } [@@deriving compare, fields, sexp]
  end

  module Headers = struct
    type t = Block.Protocol_header.t list [@@deriving compare, sexp]
  end

  module Nonce = struct
    type t =
      { nonce : int64
      } [@@deriving compare, fields, sexp]
  end

  module Reject = struct
    module Reason = struct
      type t =
        | Malformed
        | Invalid
        | Obsolete
        | Duplicate
        | Nonstandard
        | Dust
        | InsufficientFee
        | Checkpoint
        | Unknown of int
      [@@deriving compare, sexp]

      let of_int = function
        | 0x01 -> Malformed
        | 0x10 -> Invalid
        | 0x11 -> Obsolete
        | 0x12 -> Duplicate
        | 0x40 -> Nonstandard
        | 0x41 -> Dust
        | 0x42 -> InsufficientFee
        | 0x43 -> Checkpoint
        | i    -> Unknown i
      ;;

      let to_int = function
        | Malformed       -> 0x01
        | Invalid         -> 0x10
        | Obsolete        -> 0x11
        | Duplicate       -> 0x12
        | Nonstandard     -> 0x40
        | Dust            -> 0x41
        | InsufficientFee -> 0x42
        | Checkpoint      -> 0x43
        | Unknown i       -> i
      ;;
    end

    type t =
      { message : string
      ; code    : Reason.t
      ; reason  : string
      } [@@deriving compare, fields, sexp]
  end

  type t =
    | Version    of Version.t
    | VerAck
    | Addr       of Addr.t
    | GetAddr
    | Inv        of Inventory.t
    | GetData    of Inventory.t
    | NotFound   of Inventory.t
    | GetBlocks  of Getblocks.t
    | GetHeaders of Getblocks.t
    | Tx         of Transaction.t
    | Block      of Block.t
    | Headers    of Headers.t
    | MemPool
    | Ping       of Nonce.t
    | Pong       of Nonce.t
    | Reject     of Reject.t
    | Alert      of Bitstring.t (* TODO: actually parse alerts and verify the signature *)
    | Unknown    of Bitstring.t
  [@@deriving compare, sexp]

  let to_command : t -> Command.t = function
    | Version _    -> Version
    | VerAck       -> VerAck
    | Addr _       -> Addr
    | GetAddr      -> GetAddr
    | Inv _        -> Inv
    | GetData _    -> GetData
    | NotFound _   -> NotFound
    | GetBlocks _  -> GetBlocks
    | GetHeaders _ -> GetHeaders
    | Tx _         -> Tx
    | Block _      -> Block
    | Headers _    -> Headers
    | MemPool      -> MemPool
    | Ping _       -> Ping
    | Pong _       -> Pong
    | Reject _     -> Reject
    | Alert _      -> Alert
    | Unknown _    -> Unknown "UNKNOWN"
end

module Message = struct
  type t =
    { network : Magic.t
    ; payload : Messages.t
    } [@@deriving compare, fields, sexp]
end
