open! Core.Std
open Bitcaml_utils.Std
open Common

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
          ~receiver_address:(Network_address.of_bitstring receiver_address |> fst)
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
              sender_address = Some (Network_address.of_bitstring sender_address |> fst)
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
       ; rest  :  -1 : bitstring
      |} ->
      { t with relay = Some (relay > 0) }, rest
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
      t, bits
  ;;

  let to_bitstring =
    let default_sender_address =
      let host =
        String.make 16 '\x00'
        |> Bitstring.bitstring_of_string
      in
      Network_address.create
        ~services:Service.Set.empty
        ~host
        ~port:0
    in
    fun t ->
      let node_services = Service.to_int64 t.node_services in
      let timestamp = Time.to_epoch t.timestamp |> Int64.of_float in
      let receiver_address = Network_address.to_bitstring t.receiver_address in
      let sender_address =
        Option.value t.sender_address ~default:default_sender_address
        |> Network_address.to_bitstring
      in
      let random_nonce = Option.value ~default:0L t.random_nonce in
      let user_agent =
        Option.value ~default:"" t.user_agent
        |> Varstring.to_bitstring
      in
      let start_height = Option.value ~default:0l t.start_height in
      let relay =
        Option.value t.relay ~default:false
        |> Bool.to_int
      in
      [%bitstring
        {| t.protocol_version : 4*8 : littleendian
         ; node_services      : 8*8 : littleendian
         ; timestamp          : 8*8 : littleendian
         ; receiver_address   :  -1 : bitstring
	 ; sender_address     :  -1 : bitstring
	 ; random_nonce       : 8*8 : littleendian
	 ; user_agent         :  -1 : bitstring
	 ; start_height       : 4*8 : littleendian
	 ; relay              : 1*8 : littleendian
        |}]
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
            ~address:(Network_address.of_bitstring network_address |> fst)
        in
        t, rest
      | {| _ |} -> failwith "invalid addr item"
    ;;

    let to_bitstring t =
      let timestamp =
        Option.value_map t.timestamp ~default:0l ~f:(fun timestamp ->
            Time.to_epoch timestamp
            |> Int32.of_float)
      in
      let address = Network_address.to_bitstring t.address in
      [%bitstring
        {| timestamp : 4*8 : littleendian
         ; address   :  -1 : bitstring
        |}]
    ;;
  end

  type t = Item.t list [@@deriving compare, sexp]

  let of_bitstring ~protocol_version bits =
    let includes_timestamp = protocol_version >= 31402l in
    Varlist.of_bitstring
      ~element_of_bitstring:(Item.of_bitstring ~includes_timestamp)
      bits
  ;;

  let to_bitstring = Varlist.to_bitstring ~bitstring_of_element:Item.to_bitstring
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

    let to_bitstring t =
      let type_ = Type.to_int32 t.type_ in
      [%bitstring
        {| type_  : 4*8 : littleendian
         ; t.hash : 32*8 : string
        |}]
    ;;
  end

  type t = Item.t list [@@deriving compare, sexp]

  let of_bitstring bits =
    Varlist.of_bitstring
      ~element_of_bitstring:Item.of_bitstring
      bits
  ;;

  let to_bitstring = Varlist.to_bitstring ~bitstring_of_element:Item.to_bitstring
end

module Getblocks = struct
  type t =
    { version   : int32
    ; hashes    : string list
    ; hash_stop : string
    } [@@deriving compare, fields, sexp]

  let of_bitstring bits =
    let version, bits =
      match%bitstring bits with
      | {| version : 4*8 : littleendian
	 ; bits    :  -1 : bitstring
        |} -> version, bits
      | {| _ |} -> failwith "invalid block locator"
    in
    let hashes, bits =
      Varlist.of_bitstring
        ~element_of_bitstring:(Varlist.fixed_length_string_of_bitstring ~name:"block locator hash" ~length:32)
        bits
    in
    match%bitstring bits with
    | {| hash_stop : 32*8 : string
       ; rest      :   -1 : bitstring
      |} ->
      let t =
        Fields.create
          ~version
          ~hashes
          ~hash_stop
      in
      t, rest
    | {| _ |} -> failwith "invalid block locator"
  ;;

  let to_bitstring t =
    let hashes =
      Varlist.to_bitstring
        ~bitstring_of_element:(Varlist.fixed_length_string_to_bitstring ~length:32)
        t.hashes
    in
    [%bitstring
      {| t.version   :  4*8 : littleendian
       ; hashes      :   -1 : bitstring
       ; t.hash_stop : 32*8 : string
      |}]
  ;;
end

module Headers = struct
  type t = Block.Protocol_header.t list [@@deriving compare, sexp]

  let of_bitstring bits =
    Varlist.of_bitstring ~element_of_bitstring:Block.Protocol_header.of_bitstring bits
  ;;

  let to_bitstring =
    Varlist.to_bitstring
      ~bitstring_of_element:Block.Protocol_header.to_bitstring
  ;;
end

module Nonce = struct
  type t =
    { nonce : int64
    } [@@deriving compare, fields, sexp]

  let of_bitstring = function%bitstring
    | {| nonce : 8*8 : littleendian
       ; rest  :  -1 : bitstring
      |} ->
      Fields.create ~nonce, rest
    | {| _ |} -> failwith "invalid nonce message"
  ;;

  let to_bitstring t =
    [%bitstring {| t.nonce : 8*8 : littleendian |}]
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
    let t =
      Fields.create
        ~message
        ~code:(Reason.of_int code)
        ~reason
        ~data
    in
    t, Bitstring.empty_bitstring
  ;;

  let to_bitstring t =
    let message = Varstring.to_bitstring t.message in
    let code    = Reason.to_int t.code in
    [%bitstring
      {| message  :  -1 : bitstring
       ; code     : 1*8 : littleendian
       ; t.reason :  -1 : string
       ; t.data   :  -1 : bitstring
      |}]
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

module Payload = struct
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
    | Version _                    -> Version
    | VerAck                       -> VerAck
    | Addr _                       -> Addr
    | GetAddr                      -> GetAddr
    | Inv _                        -> Inv
    | GetData _                    -> GetData
    | NotFound _                   -> NotFound
    | GetBlocks _                  -> GetBlocks
    | GetHeaders _                 -> GetHeaders
    | Tx _                         -> Tx
    | Block _                      -> Block
    | Headers _                    -> Headers
    | MemPool                      -> MemPool
    | Ping _                       -> Ping
    | Pong _                       -> Pong
    | Reject _                     -> Reject
    | Alert _                      -> Alert
    | FilterLoad _                 -> FilterLoad
    | FilterAdd _                  -> FilterAdd
    | FilterClear _                -> FilterClear
    | MerkleBlock _                -> MerkleBlock
    | Other {command; payload = _} -> Other command
  ;;

  let of_bitstring ~protocol_version ~command bits : t =
    match (command : Command.t) with
    | Version       -> Version     (Version.of_bitstring bits |> fst)
    | VerAck        -> VerAck
    | Addr          -> Addr        (Addr.of_bitstring ~protocol_version bits |> fst)
    | GetAddr       -> GetAddr
    | Inv           -> Inv         (Inventory.of_bitstring bits |> fst)
    | GetData       -> GetData     (Inventory.of_bitstring bits |> fst)
    | NotFound      -> NotFound    (Inventory.of_bitstring bits |> fst)
    | GetBlocks     -> GetBlocks   (Getblocks.of_bitstring bits |> fst)
    | GetHeaders    -> GetHeaders  (Getblocks.of_bitstring bits |> fst)
    | Tx            -> Tx          (Transaction.of_bitstring bits |> fst)
    | Block         -> Block       (Block.of_bitstring bits |> fst)
    | Headers       -> Headers     (Headers.of_bitstring bits |> fst)
    | MemPool       -> MemPool
    | Ping          -> Ping        (Nonce.of_bitstring bits |> fst)
    | Pong          -> Pong        (Nonce.of_bitstring bits |> fst)
    | Reject        -> Reject      (Reject.of_bitstring bits |> fst)
    | Alert         -> Alert       bits
    | FilterLoad    -> FilterLoad  bits
    | FilterAdd     -> FilterAdd   bits
    | FilterClear   -> FilterClear bits
    | MerkleBlock   -> MerkleBlock bits
    | Other command -> Other       {command; payload = bits}
  ;;

  let to_bitstring = function
    | Version t                    -> Version.to_bitstring t
    | VerAck                       -> Bitstring.empty_bitstring
    | Addr t                       -> Addr.to_bitstring t
    | GetAddr                      -> Bitstring.empty_bitstring
    | Inv t                        -> Inventory.to_bitstring t
    | GetData t                    -> Inventory.to_bitstring t
    | NotFound t                   -> Inventory.to_bitstring t
    | GetBlocks t                  -> Getblocks.to_bitstring t
    | GetHeaders t                 -> Getblocks.to_bitstring t
    | Tx t                         -> Transaction.to_bitstring t
    | Block t                      -> Block.to_bitstring t
    | Headers t                    -> Headers.to_bitstring t
    | MemPool                      -> Bitstring.empty_bitstring
    | Ping t                       -> Nonce.to_bitstring t
    | Pong t                       -> Nonce.to_bitstring t
    | Reject t                     -> Reject.to_bitstring t
    | Alert t                      -> t
    | FilterLoad t                 -> t
    | FilterAdd t                  -> t
    | FilterClear t                -> t
    | MerkleBlock t                -> t
    | Other {payload; command = _} -> payload
  ;;
end

type t =
  { network : Magic.t
  ; payload : Payload.t
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
     ; rest     :                          -1 : bitstring
    |} ->
    begin match verify_checksum ~checksum ~payload with
      | false -> failwith "invalid message: checksum verification failed"
      | true  ->
        let command = Command.of_string command in
        let payload = Payload.of_bitstring ~protocol_version ~command payload in
        let t =
          Fields.create
            ~network:(Magic.of_int32 magic)
            ~payload
        in
        t, rest
    end
  | {| _ |} -> failwith "invalid message"
;;

let to_bitstring t =
  let magic = Magic.to_int32 t.network in
  let command =
    Payload.to_command t.payload
    |> Command.to_string
    |> Utils.zeropad_string ~length:12
  in
  let payload = Payload.to_bitstring t.payload in
  let length =
    Bitstring.bitstring_length payload
    |> Int32.of_int_exn
  in
  let checksum =
    Bitstring.string_of_bitstring payload
    |> Bitcoin_crypto.Std.Hashing.message_checksum
  in
  [%bitstring
    {| magic    :  4*8 : littleendian
     ; command  : 12*8 : string
     ; length   :  4*8 : littleendian
     ; checksum :  4*8 : string
     ; payload  :   -1 : bitstring
    |}]
;;
