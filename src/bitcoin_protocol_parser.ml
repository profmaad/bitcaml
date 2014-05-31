open Bitcoin_protocol;;
open Bitstring;;

let parse_bitcoin_header bits =
  bitmatch bits with
  | { magic : 4*8 : littleendian;
      command : 12*8 : string;
      payload_length : 4*8 : littleendian;
      checksum : 4*8 : string
    } ->
    {
      magic = bitcoin_magic_of_int (Int32.to_int magic);
      command = bitcoin_command_of_string (Utils.string_from_zeroterminated_string command);
      payload_length = (Int32.to_int payload_length);
      checksum = checksum;
    }
  | { _ } -> Printf.eprintf "Received invalid bitcoin protocol header"; raise (Invalid_argument "Invalid bitcoin protocol header")
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
let parse_bitcoin_version_message bits =
  bitmatch bits with
  | { version : 4*8 : littleendian;
      services : 8*8 : littleendian;
      timestamp : 8*8 : littleendian;
      addr_recv : 26*8 : bitstring;
      addr_from : (if version >= (Int32.of_int 106) then 26*8 else 0) : bitstring;
      nonce : (if version >= (Int32.of_int 106) then 8*8 else 0) : string;
      user_agent : ((bitstring_length bits) - (4+8+8+26+26+8+4 + (if version >= (Int32.of_int 70001) then 1 else 0))*8) : string;
      start_height : (if version >= (Int32.of_int 106) then 4*8 else 0) : littleendian;
      relay : (if version >= (Int32.of_int 70001) then 8 else 0) : littleendian
    } ->
    let receiver_address = parse_bitcoin_network_address addr_recv in
    let sender_address = parse_bitcoin_network_address addr_from in
    if Option.is_none receiver_address || Option.is_none sender_address then None
    else
      Some (VersionPayload {
	protocol_version = Int32.to_int version;
	services = bitcoin_services_set_of_int64 services;
	timestamp = Unix.localtime (Int64.to_float timestamp);
	receiver_address = Option.get receiver_address;
	sender_address = Some (Option.get receiver_address);
	random_nonce = Some nonce;
	user_agent = Some (String.sub user_agent 1 ((String.length user_agent) - 1));
	start_height = Some (Int64.to_int start_height);
	relay = Some (relay > 0L);
      })
  | { _ } -> None
;;

let parse_bitcoin_payload payload_string = function
  | VersionCommand -> parse_bitcoin_version_message (Bitstring.bitstring_of_string payload_string)
  | VerAckCommand -> Some VerAckPayload
  | _ -> Some (UnknownPayload payload_string)
;;

let read_string_from_fd fd bytes =
  let received_string = String.make bytes '\x00' in
  let bytes_read = Unix.read fd received_string 0 bytes in
  String.sub received_string 0 bytes_read
;;

let parse_bitcoin_header_from_fd fd =
  let bytestring = read_string_from_fd fd (4+12+4+4) in
  parse_bitcoin_header (Bitstring.bitstring_of_string bytestring)
;;

let read_payload_from_fd fd header =
  read_string_from_fd fd header.payload_length
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
