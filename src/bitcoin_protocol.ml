(* Magic value describing the network to use *)
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
type service = 
| NetworkNodeService
;;

(* Set representation of the services bitfield *)
module ServiceSet = Set.Make(struct
  type t = service
  let compare = Pervasives.compare
end);;

type network_address = 
  {
    services : ServiceSet.t;
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
    node_services : ServiceSet.t;
    timestamp : Unix.tm;
    receiver_address : network_address;
    sender_address : network_address option;
    random_nonce : string option;
    user_agent : string option;
    start_height : int option;
    relay : bool option;
  };;

type timestamped_network_address =
  {
    address_timestamp : Unix.tm option;
    network_address : network_address;
  }

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
  }

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
| UnknownPayload of Bitstring.t
;;

type message =
  {
    network : magic;
    payload : message_payload;
  };;

let message_checksum payload =
  let digest = Bitcoin_crypto.double_sha256 payload in
  String.sub digest 0 4
;;

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
  | VersionPayload p -> VersionCommand
  | VerAckPayload -> VerAckCommand
  | AddrPayload p -> AddrCommand
  | GetAddrPayload -> GetAddrCommand
  | InvPayload p -> InvCommand
  | GetDataPayload p -> GetDataCommand
  | NotFoundPayload p -> NotFoundCommand
  | GetBlocksPayload p -> GetBlocksCommand
  | GetHeadersPayload p -> GetHeadersCommand
  | UnknownPayload p -> UnknownCommand "UNKNOWN"

let services_set_of_int64 i = 
  let services_list = ref [] in
  if (Int64.logand i 0x0000000000000001L) > 0L then services_list := NetworkNodeService :: !services_list;
  List.fold_right ServiceSet.add !services_list ServiceSet.empty
;;
let int64_of_services_set set =
  let int64_of_service = function
    | NetworkNodeService -> 0x0000000000000001L
  in
  List.fold_left Int64.logor Int64.zero (List.map int64_of_service (ServiceSet.elements set))
;;

let inventory_item_type_of_int32 = function
  | 0x1l -> TransactionInventoryItem
  | 0x2l -> BlockInventoryItem
  | i -> UnknownInventoryItem (Int32.to_int i)
let int32_of_inventory_item_type = function
  | TransactionInventoryItem -> 0x1l
  | BlockInventoryItem -> 0x2l
  | UnknownInventoryItem i -> (Int32.of_int i)
