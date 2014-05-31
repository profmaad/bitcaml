open Unix
open Printf
open Bitstring

let may f = function
  | None -> ()
  | Some x -> f x
;;
let is_none = function
  | None -> true
  | Some x -> false
;;

exception No_value;;
let get = function
  | None -> raise No_value
  | Some x -> x
;;

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
type bitcoin_service = 
| NetworkNodeService
;;

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
    header : bitcoin_protocol_header;
    payload : bitcoin_message_payload;
  };;

let testnet_port = 18333
let peer_ip_address = "127.0.0.1"
let peer_port = testnet_port

let testnet_magic = "\x0b\x11\x09\x07"
let version_command = "version"
let verack_command = "verack"

let protocol_version = 70002

let user_agent = 
  let current_time = gmtime (time ()) in
  sprintf "\x12/bitcaml:%04d%02d%02d/" (current_time.tm_year + 1900) (current_time.tm_mon + 1) current_time.tm_mday

let print_hex_string s line_length =
  let hex_iterator index c =
    if (index > 0) && (line_length > 0) && ((index mod line_length) = 0) then print_newline ();
    printf "%02x " (int_of_char c);
  in
  String.iteri hex_iterator s;
;;

let bitcoin_sha256 data =
  Sha256.to_bin (Sha256.string (Sha256.to_bin (Sha256.string data)))
;;
let bitcoin_message_checksum payload =
  let digest = bitcoin_sha256 payload in
  String.sub digest 0 4
;;

let reverse_string s =
  let rec reverse_string_acc s acc index length =
    if index >= length
    then acc
    else reverse_string_acc s ((String.make 1 s.[index]) ^ acc) (index+1) length
  in
  reverse_string_acc s "" 0 (String.length s)
;;

let bytestring_of_int i bytesize =
  let rec bytestring_of_int_ i acc byte_index =
    let shift_distance = 8*byte_index in
    let mask = 0xff lsl shift_distance in
    let masked_int = i land mask in
    let shifted_int = masked_int lsr shift_distance in
    let byte_char = Char.chr shifted_int in
    let new_acc = acc ^ (String.make 1 byte_char) in
    if byte_index = 0 then new_acc
    else bytestring_of_int_ i new_acc (byte_index-1)
  in
  bytestring_of_int_ i "" (bytesize-1)
;;
let le_bytestring_of_int i bytesize =
  reverse_string (bytestring_of_int i bytesize)
;;

let bitcoin_magic_of_int = function
  | 0xD9B4BEF9 -> MainNetwork
  | 0xDAB5BFFA -> TestNet
  | 0x0709110B -> TestNet3
  | i -> UnknownMagic i
;;
let print_bitcoin_magic = function
  | MainNetwork -> "Main Network"
  | TestNet -> "Testnet"
  | TestNet3 -> "Testnet 3"
  | UnknownMagic i -> sprintf "Unknown (%d)" i
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
let print_bitcoin_command = function
  | VersionCommand -> "Version"
  | VerAckCommand -> "Version acknowledgement"
  | AddrCommand -> "Address"
  | InvCommand -> "Inventory"
  | GetDataCommand -> "Get Data"
  | NotFoundCommand -> "Not Found"
  | GetBlocksCommand -> "Get Blocks"
  | GetHeadersCommand -> "Get Headers"
  | TxCommand -> "Transaction"
  | BlockCommand -> "Block"
  | HeadersCommand -> "Headers"
  | GetAddrCommand -> "Get Address"
  | MemPoolCommand -> "Memory Pool"
  | PingCommand -> "Ping"
  | PongCommand -> "Poong"
  | RejectCommand -> "Reject"
  | AlertCommand -> "Alert"
  | FilterLoadCommand -> "Filter Load (BIP 37)"
  | FilterAddCommand -> "Filter Add (BIP 37)"
  | FilterClearCommand -> "Filter Clear (BIP 37)"
  | MerkleBlockCommand -> "Merkle Block (BIP 37)"
  | UnknownCommand s -> sprintf "Unknown (%s)" s
;;

let print_bitcoin_header header = 
  print_endline "Bitcoin Message Header:";
  printf "\tMagic: %s\n" (print_bitcoin_magic header.magic);
  printf "\tCommand: %s\n" (print_bitcoin_command header.command);
  printf "\tPayload Size: %d bytes\n" header.payload_length;
  print_string "\tChecksum: "; print_hex_string header.checksum 0; print_newline ();
;;

let bitcoin_services_set_of_int64 i = 
  let services_list = ref [] in
  if (Int64.logand i 0x0000000000000001L) > 0L then services_list := NetworkNodeService :: !services_list;
  List.fold_right BitcoinServiceSet.add !services_list BitcoinServiceSet.empty
;;
let print_bitcoin_services_set set =
  let string_of_bitcoin_service = function
    | NetworkNodeService -> "Full Network Node"
  in
  if BitcoinServiceSet.is_empty set then "No Services"
  else String.concat ", " (List.map string_of_bitcoin_service (BitcoinServiceSet.elements set))
;;

let string_of_ipv4_address address_string =
  sprintf "%d.%d.%d.%d"
    (Char.code (address_string.[String.length address_string - 4]))
    (Char.code (address_string.[String.length address_string - 3]))
    (Char.code (address_string.[String.length address_string - 2]))
    (Char.code (address_string.[String.length address_string - 1]))
;;

let string_of_unix_tm time =
  sprintf "%04d-%02d-%02d %02d:%02d:%02d" (time.tm_year + 1900) (time.tm_mon + 1) time.tm_mday time.tm_hour time.tm_min time.tm_sec
;;
let string_of_timestamp timestamp =
  string_of_unix_tm (localtime (Int64.to_float timestamp))
;;

let string_of_bitcoin_network_address n =
  sprintf "%s:%d (%s)" (string_of_ipv4_address n.address) n.port (print_bitcoin_services_set n.services)
;;

let print_bitcoin_version_message m = 
  print_endline "Bitcoin Version Message:";
  printf "\tProtocol Version: %d\n" m.protocol_version;
  printf "\tServices: %s\n" (print_bitcoin_services_set m.services);
  printf "\tTimestamp: %s\n" (string_of_unix_tm m.timestamp);
  printf "\tReceiver: %s\n" (string_of_bitcoin_network_address m.receiver_address);
  may (fun sender_address -> printf "\tSender: %s\n" (string_of_bitcoin_network_address sender_address)) m.sender_address;
  may (fun random_nonce -> print_string "\tRandom Nonce: "; print_hex_string random_nonce 0; print_newline()) m.random_nonce;
  may (fun user_agent -> printf "\tUser Agent: %s\n" user_agent) m.user_agent;
  may (fun start_height -> printf "\tStarting block height: %d\n" start_height) m.start_height;
  may (fun relay -> printf "\tRelay transactions?: %s\n" (if relay then "No" else "Yes")) m.relay
;;

let print_bitcoin_message_payload = function
  | VersionPayload p -> print_bitcoin_version_message p
  | VerAckPayload -> ()
  | UnknownPayload s -> printf "Unknown Message Payload (%d bytes)\n" (String.length s)
;;

let print_bitcoin_message m =
  print_bitcoin_header m.header;
  print_bitcoin_message_payload m.payload;
;;

let bitcoin_timestamp () =
  let timestamp = int_of_float (Unix.time ()) in
  let string_timestamp = le_bytestring_of_int timestamp 4 in
  string_timestamp ^ (String.make 4 '\x00')
let short_bitcoin_timestamp () =
  let long_timestamp = bitcoin_timestamp () in
  String.sub long_timestamp 0 4

let construct_bitcoin_message command payload =
  let magic = testnet_magic in
  let padded_command = command ^ String.make (12 - String.length command) '\x00' in
  let length = le_bytestring_of_int (String.length payload) 4 in
  let checksum = bitcoin_message_checksum payload in
  magic ^ padded_command ^ length ^ checksum ^ payload

let bitcoin_network_addr addr port =
  (* (short_bitcoin_timestamp ()) ^ *)
    "\x01\x00\x00\x00\x00\x00\x00\x00" ^
    "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" ^ addr ^
    (bytestring_of_int port 2)
;;

let test_version_message () =
  let payload = (le_bytestring_of_int protocol_version 4) ^ (*version*)
    "\x01\x00\x00\x00\x00\x00\x00\x00" ^ (*services*)
    (bitcoin_timestamp ()) ^ (*timestamp*)
    (bitcoin_network_addr "\xff\xff\x7f\x00\x00\x01" peer_port) ^ (*receiver netaddr struct*)
    (bitcoin_network_addr "\xff\xff\x7f\x00\x00\x01" peer_port) ^ (*sender netaddr struct*)
    "\x00\x00\xde\xad\xbe\xef\x00\x00" ^ (*"random" node nonce*)
    user_agent ^ (*empty user agent string*)
    "\x00\x00\x00\x00" ^ (*last received block*)
    "\x00" (*don't relay transactions to us*)
  in
  construct_bitcoin_message version_command payload
;;
let test_verack_message () =
  construct_bitcoin_message verack_command ""

let send_message socket message =
  let bytes_written = write socket message 0 (String.length message) in
  printf "Send message (%d bytes):\n" bytes_written;
  print_hex_string message 16;
  print_newline ();
;;
let receive_message socket =
  let received_message = String.make 4096 '\x00' in
  let bytes_read = read socket received_message 0 (String.length received_message) in
  printf "Received message (%d bytes):\n" bytes_read;
  print_hex_string (String.sub received_message 0 bytes_read) 16;
  print_newline ();
  received_message
;;

let read_string_from_fd fd bytes =
  let received_string = String.make bytes '\x00' in
  let bytes_read = read fd received_string 0 bytes in
  String.sub received_string 0 bytes_read
;;

let string_from_zeroterminated_string zts =
  let string_length =
    try
      String.index zts '\x00'
    with Not_found -> 12
  in
  String.sub zts 0 string_length
;;

let parse_bitcoin_header_from_string s =
  let header_bits = bitstring_of_string s in
  bitmatch header_bits with
  | { magic : 4*8 : littleendian;
      command : 12*8 : string;
      payload_length : 4*8 : littleendian;
      checksum : 4*8 : string
    } ->
    {
      magic = bitcoin_magic_of_int (Int32.to_int magic);
      command = bitcoin_command_of_string (string_from_zeroterminated_string command);
      payload_length = (Int32.to_int payload_length);
      checksum = checksum;
    }
  | { _ } -> eprintf "Received invalid bitcoin protocol header"; raise (Invalid_argument "Invalid bitcoin protocol header")
;;
let parse_bitcoin_header_from_fd fd =
  let bytestring = read_string_from_fd fd (4+12+4+4) in
  parse_bitcoin_header_from_string bytestring
;;

let parse_bitcoin_network_address bits =
  bitmatch bits with 
  | { services : 8*8 : littleendian;
      address : 16*8 : string;
      port : 2*8 : bigendian
    } ->
    Some {
      services = bitcoin_services_set_of_int64 services;
      address = address;
      port = port;
    }
  | { _ } -> None
;;

(* TODO: var_int parser. turns out, you can correctly parse a var_int by only looking at the initial byte *)

(* TODO: we should instead use two or three separate patterns, or parse them in sequence *)
let parse_bitcoin_version_message_from_string s =
  let message_bits = bitstring_of_string s in
  bitmatch message_bits with
  | { version : 4*8 : littleendian;
      services : 8*8 : littleendian;
      timestamp : 8*8 : littleendian;
      addr_recv : 26*8 : bitstring;
      addr_from : (if version >= (Int32.of_int 106) then 26*8 else 0) : bitstring;
      nonce : (if version >= (Int32.of_int 106) then 8*8 else 0) : string;
      user_agent : ((String.length s) - (4+8+8+26+26+8+4 + (if version >= (Int32.of_int 70001) then 1 else 0)))*8 : string;
      start_height : (if version >= (Int32.of_int 106) then 4*8 else 0) : littleendian;
      relay : (if version >= (Int32.of_int 70001) then 8 else 0) : littleendian
    } ->
    let receiver_address = parse_bitcoin_network_address addr_recv in
    let sender_address = parse_bitcoin_network_address addr_from in
    if is_none receiver_address || is_none sender_address then None
    else
      Some (VersionPayload {
	protocol_version = Int32.to_int version;
	services = bitcoin_services_set_of_int64 services;
	timestamp = localtime (Int64.to_float timestamp);
	receiver_address = get receiver_address;
	sender_address = Some (get receiver_address);
	random_nonce = Some nonce;
	user_agent = Some (String.sub user_agent 1 ((String.length user_agent) - 1));
	start_height = Some (Int64.to_int start_height);
	relay = Some (relay > 0L);
      })
  | { _ } -> None
;;
let read_payload_from_fd fd header =
  read_string_from_fd fd header.payload_length
;;
let parse_bitcoin_payload payload_string = function
  | VersionCommand -> parse_bitcoin_version_message_from_string payload_string
  | VerAckCommand -> Some VerAckPayload
  | _ -> Some (UnknownPayload payload_string)
;;

let verify_bitcoin_message_checksum header payload_string  =
  let payload_checksum = bitcoin_message_checksum payload_string in
  header.checksum = payload_checksum
;;

let read_and_parse_bitcoin_message_from_fd fd =
  let header = parse_bitcoin_header_from_fd fd in
  let payload_string = read_payload_from_fd fd header in
  if not (verify_bitcoin_message_checksum header payload_string) then None
  else
    let payload = parse_bitcoin_payload payload_string header.command in
    match payload with
    | None -> None
    | Some payload -> Some { header = header; payload = payload }
;;

(* main *)
let main =
  let client_socket = socket PF_INET SOCK_STREAM 0 in
  let peer_addr = ADDR_INET(inet_addr_of_string peer_ip_address, peer_port) in
  connect client_socket peer_addr;
  send_message client_socket (test_version_message ());
  let received_message = read_and_parse_bitcoin_message_from_fd client_socket in
  may print_bitcoin_message received_message;
  send_message client_socket (test_verack_message ());
  close client_socket
;;
  

