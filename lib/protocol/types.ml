open! Core.Std
open Bitcaml_utils.Std

module type Bitstringable = sig
  type t

  (* val to_bitstring : t -> Bitstring.t *)
  val of_bitstring : Bitstring.t -> t
end

module Varint = struct
  type t = int64 [@@deriving compare, sexp]

  let of_bitstring bits =
    let parse_value bits size =
      match%bitstring bits with
      | {| value : size*8 : littleendian
	 ; rest   :    -1 : bitstring
        |}      -> value, rest
      | {| _ |} -> failwith "invalid varint"
    in
    match%bitstring bits with
    | {| tag  : 1*8 : littleendian
       ; rest :  -1 : bitstring
      |} -> begin
        match tag with
        | 0xff -> parse_value bits 8
        | 0xfe -> parse_value bits 4
        | 0xfd -> parse_value bits 2
        | i    -> Int64.of_int i, rest
      end
    | {| _ |} -> failwith "invalid varint"
  ;;
end

module Varstring = struct
  type t = string [@@deriving compare, sexp]

  (* we should support strings with the full length of MAX(int64)
     bytes, but due to bitstring requiring the length in BITS in an
     OCaml int (31 bits, signed), we can only support much much
     shorter strings *facepalm* *)
  let of_bitstring ~name bits =
    let length, bits = Varint.of_bitstring bits in
    let length = Int64.to_int_exn length * 8 in
      match%bitstring bits with
      | {| value : length : string
	 ; rest  :     -1 : bitstring
        |} -> value, rest
      | {| _ |} -> failwithf "invalid %s" name ()
  ;;
end

module Varlist = struct
  let fixed_length_string_item ~name ~length = function%bitstring
    | {| value : length*8 : string
       ; rest  :       -1 : bitstring
      |} -> value, rest
    | {| _ |} -> failwithf "invalid %s list item" name ()
  ;;

  let of_bitstring ~element_of_bitstring bits =
    let rec elements acc bits = function
      | 0L -> acc, bits
      | n ->
        let element, bits = element_of_bitstring bits in
        elements (element :: acc) bits (Int64.pred n)
    in
    let count, bits = Varint.of_bitstring bits in
    let elements, rest = elements [] bits count in
    List.rev elements, rest
    ;;
end

module Magic = struct
  type t =
    | MainNetwork
    | TestNet
    | TestNet3
    | Other of int32
  [@@deriving compare, sexp]

  let of_int32 = function
    | 0xD9B4BEF9l -> MainNetwork
    | 0xDAB5BFFAl -> TestNet
    | 0x0709110Bl -> TestNet3
    | i           -> Other i
  ;;

  let to_int32 = function
    | MainNetwork -> 0xD9B4BEF9l
    | TestNet     -> 0xDAB5BFFAl
    | TestNet3    -> 0x0709110Bl
    | Other i     -> i
  ;;
end

module Command = struct
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

  let of_string s =
    Utils.string_of_zeroterminated_string s
    |> function
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
    | s             -> Other s
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
    | Other s     -> s
  ;;
end

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
    ; address  : Host_and_port.t
    } [@@deriving compare, fields, sexp]

  let of_bitstring = function%bitstring
    | {| services :  8*8 : littleendian
       ; host     : 16*8 : string
       ; port     :  2*8 : bigendian
      |} ->
      Fields.create
        ~services:(Service.of_int64 services)
        ~address:(Host_and_port.create ~host ~port)
    | {| _ |} -> failwith "invalid network address"
;;
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

    let to_int32 = function
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

    let of_bitstring = function%bitstring
      | {| hash  : 32*8 : string
	 ; index :  4*8 : littleendian
	 ; rest  :   -1 : bitstring
	|} ->
        let t =
          Fields.create
            ~referenced_transaction_hash:hash
            ~index
        in
        t, rest
      | {| _ |} -> failwith "invalid transaction outpoint"
    ;;
  end

  module Input = struct
    type t =
      { previous_output  : Outpoint.t
      ; signature_script : string
      ; sequence_number  : int32
      } [@@deriving compare, fields, sexp]

    let of_bitstring bits =
      let previous_output, bits = Outpoint.of_bitstring bits in
      let signature_script, bits = Varstring.of_bitstring ~name:"transaction input" bits in
      match%bitstring bits with
      | {| sequence_number : 4*8 : littleendian
	 ; rest            :  -1 : bitstring
	|} ->
        let t =
          Fields.create
            ~previous_output
            ~signature_script
            ~sequence_number
        in
        t, rest
      | {| _ |} -> failwith "invalid transaction input"
    ;;
  end

  module Output = struct
    type t =
      { value  : int64
      ; script : string
      } [@@deriving compare, fields, sexp]

    let of_bitstring bits =
      let value, bits =
        match%bitstring bits with
        | {| value : 8*8 : littleendian
           ; bits  :  -1 : bitstring
          |} -> value, bits
        | {| _ |} -> failwith "invalid transaction output"
      in
      let script, rest = Varstring.of_bitstring ~name:"transaction output script" bits in
      let t =
        Fields.create
          ~value
          ~script
      in
      t, rest
    ;;
  end

  type t =
    { version   : int32
    ; inputs    : Input.t list
    ; outputs   : Output.t list
    ; lock_time : Lock_time.t
    } [@@deriving compare, fields, sexp]

  let of_bitstring' bits =
    let version, bits =
      match%bitstring bits with
      | {| version : 4*8 : littleendian
         ; bits    :  -1 : bitstring
        |} -> version, bits
      | {| _ |} -> failwith "invalid transaction"
    in
    let inputs , bits = Varlist.of_bitstring ~element_of_bitstring:Input.of_bitstring  bits in
    let outputs, bits = Varlist.of_bitstring ~element_of_bitstring:Output.of_bitstring bits in
    match%bitstring bits with
    | {| lock_time : 4*8 : littleendian
       ; rest      :  -1 : bitstring
      |} ->
      let t =
        Fields.create
          ~version
          ~inputs
          ~outputs
          ~lock_time:(Lock_time.of_int32 lock_time)
      in
      t, rest
    | {| _ |} -> failwith "invalid transaction"
  ;;

  let of_bitstring bits =
    let t, (_ : Bitstring.t) = of_bitstring' bits in
    t
  ;;
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

    let of_bitstring = function%bitstring
      | {| version             :  4*8 : littleendian
         ; previous_block_hash : 32*8 : string
         ; merkle_root         : 32*8 : string
         ; timestamp           :  4*8 : littleendian
         ; difficulty_target   :  4*8 : littleendian
         ; nonce               :  4*8 : littleendian
         ; rest                :   -1 : bitstring
        |} ->
        let t =
          Fields.create
            ~version
            ~previous_block_hash
            ~merkle_root
            ~timestamp:(Int32.to_float timestamp |> Time.of_epoch)
            ~difficulty_target:(Difficulty.of_int32 difficulty_target)
            ~nonce
        in
        t, rest
      | {| _ |} -> failwith "invalid block header"
  end

  module Protocol_header = struct
    type t =
      { header            : Header.t
      ; transaction_count : int64
      } [@@deriving compare, fields, sexp]

    let of_bitstring bits =
      let header, bits = Header.of_bitstring bits in
      let transaction_count, rest = Varint.of_bitstring bits in
      let t =
        Fields.create
          ~header
          ~transaction_count
      in
      t, rest
    ;;
  end

  type t =
    { header       : Header.t
    ; transactions : Transaction.t list
    } [@@deriving compare, fields, sexp]

  let of_bitstring' bits =
    let header, bits = Header.of_bitstring bits in
    let transactions, rest =
      Varlist.of_bitstring
        ~element_of_bitstring:Transaction.of_bitstring'
        bits
    in
    let t =
      Fields.create
        ~header
        ~transactions
    in
    t, rest
  ;;

  let of_bitstring bits =
    let t, (_ : Bitstring.t) = of_bitstring' bits in
    t
  ;;
end

module Messages = struct
  module Version = struct
    type t =
      { protocol_version : int32
      ; node_services    : Service.Set.t
      ; timestamp        : Time.t
      ; receiver_address : Network_address.t
      ; sender_address   : Network_address.t option
      ; random_nonce     : int64 option
      ; user_agent       : string option
      ; start_height     : int32 option
      ; relay            : bool option
      } [@@deriving compare, fields, sexp]

    let of_bitstring_v0 = function%bitstring
      | {| protocol_version :  4*8 : littleendian
	 ; services         :  8*8 : littleendian
	 ; timestamp        :  8*8 : littleendian
	 ; receiver_address : 26*8 : bitstring
	 ; bits             :   -1 : bitstring
        |} ->
        let t =
          Fields.create
            ~protocol_version
            ~node_services:(Service.of_int64 services)
            ~timestamp:(Int64.to_float timestamp |> Time.of_epoch)
            ~receiver_address:(Network_address.of_bitstring receiver_address)
            ~sender_address:None
            ~random_nonce:None
            ~user_agent:None
            ~start_height:None
            ~relay:None
        in
        t, bits
      | {| _ |} -> failwith "invalid version message"
    ;;

    let of_bitstring_v106 t = function%bitstring
      | {| sender_address : 26*8 : bitstring
	 ; random_nonce   :  8*8 : littleendian
         ; bits           :   -1 : bitstring
        |} -> begin
          let user_agent, bits = Varstring.of_bitstring ~name:"version message" bits in
          match%bitstring bits with
          | {| start_height : 4*8 : littleendian
	     ; bits         :  -1 : bitstring
            |} ->
            let t =
              { t with
                sender_address = Some (Network_address.of_bitstring sender_address)
              ; random_nonce   = Some random_nonce
              ; user_agent     = Some user_agent
              ; start_height   = Some start_height
              }
            in
            t, bits
        end
      | {| _ |} -> failwith "invalid version message"
    ;;

    let of_bitstring_v70001 t = function%bitstring
      | {| relay : 1*8 : littleendian
	 ; _     :  -1 : bitstring
        |} ->
        { t with relay = Some (relay > 0) }
      | {| _ |} -> failwith "invalid version message"
    ;;

    let of_bitstring bits =
      let t, bits = of_bitstring_v0 bits in
      let t, bits =
        if protocol_version t > 106l then
          of_bitstring_v106 t bits
        else
          t, bits
      in
      if protocol_version t > 70001l then
        of_bitstring_v70001 t bits
      else
        t
    ;;
  end

  module Addr = struct
    module Item = struct
      type t =
        { timestamp : Time.t option
        ; address   : Network_address.t
        } [@@deriving compare, fields, sexp]

      let of_bitstring ~includes_timestamp bits =
        let timestamp_length = if includes_timestamp then 4*8 else 0 in
        match%bitstring bits with
        | {| timestamp       : timestamp_length : littleendian
	   ; network_address :             26*8 : bitstring
           ; rest            :               -1 : bitstring
          |} ->
          let timestamp =
            Option.some_if includes_timestamp (Int64.to_float timestamp |> Time.of_epoch)
          in
          let t =
            Fields.create
              ~timestamp
              ~address:(Network_address.of_bitstring network_address)
          in
          t, rest
        | {| _ |} -> failwith "invalid addr item"
      ;;
    end

    type t = Item.t list [@@deriving compare, sexp]

    let of_bitstring ~protocol_version bits =
      let includes_timestamp = protocol_version >= 31402 in
      Varlist.of_bitstring
        ~element_of_bitstring:(Item.of_bitstring ~includes_timestamp)
        bits
      |> fst
    ;;
  end

  module Inventory = struct
    module Item = struct
      module Type = struct
        type t =
          | Transaction
          | Block
          | Other of int32
        [@@deriving compare, sexp]

        let of_int32 = function
          | 0x1l -> Transaction
          | 0x2l -> Block
          | i    -> Other i
        ;;

        let to_int32 = function
          | Transaction -> 0x1l
          | Block       -> 0x2l
          | Other i     -> i
        ;;
      end

      type t =
        { type_ : Type.t
        ; hash  : string
        } [@@deriving compare, fields, sexp]

      let of_bitstring = function%bitstring
        | {| type_ :  4*8 : littleendian
	   ; hash  : 32*8 : string
	   ; rest  :   -1 : bitstring
	  |} ->
          let t =
            Fields.create
              ~type_:(Type.of_int32 type_)
              ~hash
          in
          t, rest
        | {| _ |} -> failwith "invalid inventory list item"
      ;;
    end

    type t = Item.t list [@@deriving compare, sexp]

    let of_bitstring bits =
      Varlist.of_bitstring
        ~element_of_bitstring:Item.of_bitstring
        bits
      |> fst
    ;;
  end

  module Getblocks = struct
    type t =
      { protocol_version : int32
      ; hashes           : string list
      ; hash_stop        : string
      } [@@deriving compare, fields, sexp]

    let of_bitstring bits =
      let protocol_version, bits =
        match%bitstring bits with
        | {| protocol_version : 4*8 : littleendian
	   ; bits             :  -1 : bitstring
          |} -> protocol_version, bits
        | {| _ |} -> failwith "invalid block locator"
      in
      let hashes, bits =
        Varlist.of_bitstring
          ~element_of_bitstring:(Varlist.fixed_length_string_item ~name:"block locator hash" ~length:32)
          bits
      in
      match%bitstring bits with
      | {| hash_stop : 32*8 : string |} ->
        Fields.create
          ~protocol_version
          ~hashes
          ~hash_stop
      | {| _ |} -> failwith "invalid block locator"
    ;;
  end

  module Headers = struct
    type t = Block.Protocol_header.t list [@@deriving compare, sexp]

    let of_bitstring bits =
      Varlist.of_bitstring ~element_of_bitstring:Block.Protocol_header.of_bitstring bits
      |> fst
    ;;
  end

  module Nonce = struct
    type t =
      { nonce : int64
      } [@@deriving compare, fields, sexp]

    let of_bitstring = function%bitstring
      | {| nonce : 8*8 : littleendian |} ->
        Fields.create ~nonce
      | {| _ |} -> failwith "invalid nonce message"
    ;;
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
        | Other of int
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
        | i    -> Other i
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
        | Other i         -> i
      ;;
    end

    type t =
      { message : string
      ; code    : Reason.t
      ; reason  : string
      ; data    : Bitstring.t
      } [@@deriving compare, fields, sexp]

    let of_bitstring bits =
      let message, bits = Varstring.of_bitstring ~name:"reject message" bits in
      let code, bits =
        match%bitstring bits with
        | {| code : 1*8 : littleendian
           ; bits :  -1 : bitstring
          |} ->
          code, bits
        | {| _ |} -> failwith "invalid reject message"
      in
      let reason, data = Varstring.of_bitstring ~name:"reject message" bits in
      Fields.create
        ~message
        ~code:(Reason.of_int code)
        ~reason
        ~data
    ;;
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
    | Alert       of Bitstring.t (* TODO: actually parse alerts and verify the signature *)
    | FilterLoad  of Bitstring.t (* TODO: parse *)
    | FilterAdd   of Bitstring.t (* TODO: parse *)
    | FilterClear of Bitstring.t (* TODO: parse *)
    | MerkleBlock of Bitstring.t (* TODO: parse *)
    | Other       of { command : string; payload: Bitstring.t }
  [@@deriving compare, sexp]

  let to_command : t -> Command.t = function
    | Version _     -> Version
    | VerAck        -> VerAck
    | Addr _        -> Addr
    | GetAddr       -> GetAddr
    | Inv _         -> Inv
    | GetData _     -> GetData
    | NotFound _    -> NotFound
    | GetBlocks _   -> GetBlocks
    | GetHeaders _  -> GetHeaders
    | Tx _          -> Tx
    | Block _       -> Block
    | Headers _     -> Headers
    | MemPool       -> MemPool
    | Ping _        -> Ping
    | Pong _        -> Pong
    | Reject _      -> Reject
    | Alert _       -> Alert
    | FilterLoad _  -> FilterLoad
    | FilterAdd _   -> FilterAdd
    | FilterClear _ -> FilterClear
    | MerkleBlock _ -> MerkleBlock
    | Other _       -> Other "UNKNOWN"
  ;;

  let of_bitstring ~protocol_version ~command bits : t =
    match (command : Command.t) with
    | Version       -> Version     (Version.of_bitstring bits)
    | VerAck        -> VerAck
    | Addr          -> Addr        (Addr.of_bitstring ~protocol_version bits)
    | GetAddr       -> GetAddr
    | Inv           -> Inv         (Inventory.of_bitstring bits)
    | GetData       -> GetData     (Inventory.of_bitstring bits)
    | NotFound      -> NotFound    (Inventory.of_bitstring bits)
    | GetBlocks     -> GetBlocks   (Getblocks.of_bitstring bits)
    | GetHeaders    -> GetHeaders  (Getblocks.of_bitstring bits)
    | Tx            -> Tx          (Transaction.of_bitstring bits)
    | Block         -> Block       (Block.of_bitstring bits)
    | Headers       -> Headers     (Headers.of_bitstring bits)
    | MemPool       -> MemPool
    | Ping          -> Ping        (Nonce.of_bitstring bits)
    | Pong          -> Pong        (Nonce.of_bitstring bits)
    | Reject        -> Reject      (Reject.of_bitstring bits)
    | Alert         -> Alert       bits
    | FilterLoad    -> FilterLoad  bits
    | FilterAdd     -> FilterAdd   bits
    | FilterClear   -> FilterClear bits
    | MerkleBlock   -> MerkleBlock bits
    | Other command -> Other       {command; payload = bits}
  ;;
end

module Message = struct
  type t =
    { network : Magic.t
    ; payload : Messages.t
    } [@@deriving compare, fields, sexp]

  let verify_checksum ~checksum ~payload =
    Bitstring.string_of_bitstring payload
    |> Bitcoin_crypto.Std.Hashing.message_checksum
    |> String.equal checksum
  ;;

  let of_bitstring ~protocol_version = function%bitstring
    | {| magic    :                         4*8 : littleendian
       ; command  :                        12*8 : string
       ; length   :                         4*8 : littleendian
       ; checksum :                         4*8 : string
       ; payload  : (Int32.to_int_exn length)*8 : bitstring
      |} ->
      begin match verify_checksum ~checksum ~payload with
        | false -> failwith "invalid message: checksum verification failed"
        | true  ->
          let command = Command.of_string command in
          let payload = Messages.of_bitstring ~protocol_version ~command payload in
          Fields.create
            ~network:(Magic.of_int32 magic)
            ~payload
      end
    | {| _ |} -> failwith "invalid message"
  ;;
end
