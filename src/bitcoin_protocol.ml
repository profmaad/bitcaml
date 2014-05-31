(* Magic value describing the network to use *)
type bitcoin_magic = 
| UnknownMagic of int
| MainNetwork
| TestNet
| TestNet3
;;

type bitcoin_command =
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
type bitcoin_service = 
| NetworkNodeService
;;

(* Set representation of the services bitfield *)
module BitcoinServiceSet = Set.Make(struct
  type t = bitcoin_service
  let compare = Pervasives.compare
end);;

type bitcoin_network_address = 
  {
    services : BitcoinServiceSet.t;
    address : string;
    port : int;
  };;

type bitcoin_protocol_header = 
  { 
    magic : bitcoin_magic;
    command : bitcoin_command;
    payload_length : int;
    checksum : string;
  };;

type bitcoin_version_message =
  {
    protocol_version : int;
    services : BitcoinServiceSet.t;
    timestamp : Unix.tm;
    receiver_address : bitcoin_network_address;
    sender_address : bitcoin_network_address option;
    random_nonce : string option;
    user_agent : string option;
    start_height : int option;
    relay : bool option;
  };;

type bitcoin_message_payload = 
| VersionPayload of bitcoin_version_message
| VerAckPayload
| UnknownPayload of string
;;

type bitcoin_message =
  {
    network : bitcoin_magic;
    payload : bitcoin_message_payload;
  };;

let bitcoin_message_checksum payload =
  let digest = Bitcoin_crypto.double_sha256 payload in
  String.sub digest 0 4
;;

let bitcoin_magic_of_int = function
  | 0xD9B4BEF9 -> MainNetwork
  | 0xDAB5BFFA -> TestNet
  | 0x0709110B -> TestNet3
  | i -> UnknownMagic i
;;

let bitcoin_command_of_string = function
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

let bitcoin_services_set_of_int64 i = 
  let services_list = ref [] in
  if (Int64.logand i 0x0000000000000001L) > 0L then services_list := NetworkNodeService :: !services_list;
  List.fold_right BitcoinServiceSet.add !services_list BitcoinServiceSet.empty
;;
