open! Core.Std
open Bitcaml_utils.Std

(* Magic value describing the network to use *)
let coin_size = 100000000L;;
let satoshis_per_bitoin = 100000000.0;;

type magic =
| UnknownMagic of int32
| MainNetwork
| TestNet
| TestNet3
;;

type command =
| UnknownCommand of string
| VersionCommand
| VerAckCommand
| AddrCommand
| InvCommand
| GetDataCommand
| NotFoundCommand
| GetBlocksCommand
| GetHeadersCommand
| TxCommand
| BlockCommand
| HeadersCommand
| GetAddrCommand
| MemPoolCommand
| PingCommand
| PongCommand
| RejectCommand
| AlertCommand
| FilterLoadCommand
| FilterAddCommand
| FilterClearCommand
| MerkleBlockCommand
;;

(* Services defined for the services field *)
module Service = struct
  module T = struct
    type t =
      | NetworkNodeService
    [@@deriving compare, sexp]
    ;;
  end
  include T

  include Comparable.Make(T)
end

type network_address =
  {
    services : Service.Set.t;
    address : string;
    port : int;
  };;

type header =
  {
    magic : magic;
    command : command;
    payload_length : int;
    checksum : string;
  };;

type version_message =
  {
    protocol_version : int;
    node_services : Service.Set.t;
    timestamp : Unix.tm;
    receiver_address : network_address;
    sender_address : network_address option;
    random_nonce : int64 option;
    user_agent : string option;
    start_height : int option;
    relay : bool option;
  };;

type timestamped_network_address =
  {
    address_timestamp : Unix.tm option;
    network_address : network_address;
  };;

type addr_message =
  {
    addresses : timestamped_network_address list;
  };;

type inventory_item_type =
| TransactionInventoryItem
| BlockInventoryItem
| UnknownInventoryItem of int
;;

type inventory_item =
  {
    inventory_item_type : inventory_item_type;
    inventory_item_hash : string;
  };;

type inventory_list_message =
  {
    inventory : inventory_item list;
  };;

type block_locator_list_message =
  {
    block_protocol_version : int;
    block_locator_hashes : string list;
    block_locator_hash_stop : string;
  };;

type transaction_lock_time =
| AlwaysLockedTransaction
| BlockLockedTransaction of int32
| TimestampLockedTransaction of Unix.tm
;;
type transaction_outpoint =
  {
    referenced_transaction_hash : string;
    transaction_output_index : int32;
  };;
type transaction_input =
  {
    previous_transaction_output : transaction_outpoint;
    signature_script : string;
    transaction_sequence_number : int32;
  };;
type transaction_output =
  {
    transaction_output_value : int64;
    output_script : string;
  };;
type transaction =
  {
    transaction_data_format_version : int;
    transaction_inputs : transaction_input list;
    transaction_outputs : transaction_output list;
    transaction_lock_time : transaction_lock_time;
  };;

type difficulty_bits =
  {
    bits_base : int;
    bits_exponent : int;
  }

type block_header =
  {
    block_version : int;
    previous_block_hash : string;
    merkle_root : string;
    block_timestamp : Unix.tm;
    block_difficulty_target : difficulty_bits;
    block_nonce : int32;
  };;
type protocol_block_header =
  {
    basic_block_header : block_header;
    block_transaction_count : int64;
  };;
type block =
  {
    block_header : block_header;
    block_transactions : transaction list;
  };;

type headers_message =
  {
    block_headers : protocol_block_header list;
  };;

type nonce_message =
  {
    message_nonce : int64;
  };;

type rejection_reason =
| RejectionMalformed
| RejectionInvalid
| RejectionObsolete
| RejectionDuplicate
| RejectionNonstandard
| RejectionDust
| RejectionInsufficientFee
| RejectionCheckpoint
| RejectionUnknown of int
;;

type reject_message =
  {
    rejected_message : string;
    rejection_code : rejection_reason;
    rejection_reason : string;
  };;

type message_payload =
| VersionPayload of version_message
| VerAckPayload
| AddrPayload of addr_message
| GetAddrPayload
| InvPayload of inventory_list_message
| GetDataPayload of inventory_list_message
| NotFoundPayload of inventory_list_message
| GetBlocksPayload of block_locator_list_message
| GetHeadersPayload of block_locator_list_message
| TxPayload of transaction
| BlockPayload of block
| HeadersPayload of headers_message
| MemPoolPayload
| PingPayload of nonce_message
| PongPayload of nonce_message
| RejectPayload of reject_message
| AlertPayload of Bitstring.t (* TODO: actually parse alerts and verify the signature *)
| UnknownPayload of Bitstring.t
;;

type message =
  {
    network : magic;
    payload : message_payload;
  };;

let magic_of_int32 = function
  | 0xD9B4BEF9l -> MainNetwork
  | 0xDAB5BFFAl -> TestNet
  | 0x0709110Bl -> TestNet3
  | i -> UnknownMagic i
;;
let int32_of_magic = function
  | MainNetwork -> 0xD9B4BEF9l
  | TestNet -> 0xDAB5BFFAl
  | TestNet3 -> 0x0709110Bl
  | UnknownMagic i -> i
;;

let command_of_string = function
  | "version" -> VersionCommand
  | "verack" -> VerAckCommand
  | "addr" -> AddrCommand
  | "inv" -> InvCommand
  | "getdata" -> GetDataCommand
  | "notfound" -> NotFoundCommand
  | "getblocks" -> GetBlocksCommand
  | "getheaders" -> GetHeadersCommand
  | "tx" -> TxCommand
  | "block" -> BlockCommand
  | "headers" -> HeadersCommand
  | "getaddr" -> GetAddrCommand
  | "mempool" -> MemPoolCommand
  | "ping" -> PingCommand
  | "pong" -> PongCommand
  | "reject" -> RejectCommand
  | "alert" -> AlertCommand
  | "filterload" -> FilterLoadCommand
  | "filteradd" -> FilterAddCommand
  | "filterclear" -> FilterClearCommand
  | "merkleblock" -> MerkleBlockCommand
  | s -> UnknownCommand s
;;
let string_of_command = function
  | VersionCommand -> "version"
  | VerAckCommand -> "verack"
  | AddrCommand -> "addr"
  | InvCommand -> "inv"
  | GetDataCommand -> "getdata"
  | NotFoundCommand -> "notfound"
  | GetBlocksCommand -> "getblocks"
  | GetHeadersCommand -> "getheaders"
  | TxCommand -> "tx"
  | BlockCommand -> "block"
  | HeadersCommand -> "headers"
  | GetAddrCommand -> "getaddr"
  | MemPoolCommand -> "mempool"
  | PingCommand -> "ping"
  | PongCommand -> "pong"
  | RejectCommand -> "reject"
  | AlertCommand -> "alert"
  | FilterLoadCommand -> "filterload"
  | FilterAddCommand -> "filteradd"
  | FilterClearCommand -> "filterclear"
  | MerkleBlockCommand -> "merkleblock"
  | UnknownCommand s -> s
;;
let command_of_message_payload = function
  | VersionPayload _ -> VersionCommand
  | VerAckPayload -> VerAckCommand
  | AddrPayload _ -> AddrCommand
  | GetAddrPayload -> GetAddrCommand
  | InvPayload _ -> InvCommand
  | GetDataPayload _ -> GetDataCommand
  | NotFoundPayload _ -> NotFoundCommand
  | GetBlocksPayload _ -> GetBlocksCommand
  | GetHeadersPayload _ -> GetHeadersCommand
  | TxPayload _ -> TxCommand
  | BlockPayload _ -> BlockCommand
  | HeadersPayload _ -> HeadersCommand
  | MemPoolPayload -> MemPoolCommand
  | PingPayload _ -> PingCommand
  | PongPayload _ -> PongCommand
  | RejectPayload _ -> RejectCommand
  | AlertPayload _ -> AlertCommand
  | UnknownPayload _ -> UnknownCommand "UNKNOWN"
;;

let services_set_of_int64 i =
  let services_list = ref [] in
  if (Int64.bit_and i 0x0000000000000001L) > 0L then services_list := Service.NetworkNodeService :: !services_list;
  List.fold ~init:Service.Set.empty ~f:Set.add !services_list
;;
let int64_of_services_set set =
  let int64_of_service = function
    | Service.NetworkNodeService -> 0x0000000000000001L
  in
  Set.to_list set
  |> List.map ~f:int64_of_service
  |> List.fold ~init:Int64.zero ~f:Int64.bit_or
;;

let inventory_item_type_of_int32 = function
  | 0x1l -> TransactionInventoryItem
  | 0x2l -> BlockInventoryItem
  | i    -> UnknownInventoryItem (Int.of_int32_exn i)
;;
let int32_of_inventory_item_type = function
  | TransactionInventoryItem -> 0x1l
  | BlockInventoryItem       -> 0x2l
  | UnknownInventoryItem i   -> (Int32.of_int_exn i)
;;

let transaction_lock_time_of_int32 = function
  | 0x0l -> AlwaysLockedTransaction
  | i when (i < 500000000l) -> BlockLockedTransaction i
  | i -> TimestampLockedTransaction (Utils.unix_tm_of_int32 i)
;;
let int32_of_transaction_lock_time = function
  | AlwaysLockedTransaction -> 0x0l
  | BlockLockedTransaction i -> i
  | TimestampLockedTransaction timestamp -> Utils.int32_of_unix_tm timestamp
;;

let rejection_reason_of_int = function
  | 0x01 -> RejectionMalformed
  | 0x10 -> RejectionInvalid
  | 0x11 -> RejectionObsolete
  | 0x12 -> RejectionDuplicate
  | 0x40 -> RejectionNonstandard
  | 0x41 -> RejectionDust
  | 0x42 -> RejectionInsufficientFee
  | 0x43 -> RejectionCheckpoint
  | i -> RejectionUnknown i
;;
let int_of_rejection_reason = function
  | RejectionMalformed -> 0x01
  | RejectionInvalid -> 0x10
  | RejectionObsolete -> 0x11
  | RejectionDuplicate -> 0x12
  | RejectionNonstandard -> 0x40
  | RejectionDust -> 0x41
  | RejectionInsufficientFee -> 0x42
  | RejectionCheckpoint -> 0x43
  | RejectionUnknown i -> i
;;

let difficulty_bits_of_int32 i =
  let exponent = Int32.shift_right_logical (Int32.bit_and i 0xff000000l) 24 in
  let base = Int32.bit_and i 0x00ffffffl in
  {
    bits_base = Int32.to_int_exn base;
    bits_exponent = Int32.to_int_exn exponent;
  }
;;
let int32_of_difficulty_bits bits =
  let exponent = Int32.shift_left (Int32.of_int_exn bits.bits_exponent) 24 in
  Int32.bit_or exponent (Int32.of_int_exn bits.bits_base)
;;
