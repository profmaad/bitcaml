open Bitcoin_protocol;;
open Bitstring;;

let parse_header bits =
  bitmatch bits with
  | { magic : 4*8 : littleendian;
      command : 12*8 : string;
      payload_length : 4*8 : littleendian;
      checksum : 4*8 : string
    } ->
    {
      magic = magic_of_int (Int32.to_int magic);
      command = command_of_string (Utils.string_from_zeroterminated_string command);
      payload_length = (Int32.to_int payload_length);
      checksum = checksum;
    }
  | { _ } -> Printf.eprintf "Received invalid bitcoin protocol header"; raise (Invalid_argument "Invalid bitcoin protocol header")
;;

let parse_network_address bits =
  bitmatch bits with 
  | { services : 8*8 : littleendian;
      address : 16*8 : string;
      port : 2*8 : bigendian
    } ->
    Some {
      services = services_set_of_int64 services;
      address = address;
      port = port;
    }
  | { _ } -> None
;;

(* TODO: var_int parser. turns out, you can correctly parse a var_int by only looking at the initial byte *)

(* This is kind of convoluted and horrible, but it's only partly our fault - the version message is kind of convoluted and horrible. *)
let parse_version_message bits =
  let parse_version_message_v0 bits =
    bitmatch bits with
    | { version : 4*8 : littleendian;
	services : 8*8 : littleendian;
	timestamp : 8*8 : littleendian;
	addr_recv : 26*8 : bitstring;
	rest : -1 : bitstring
      } ->
      let receiver_address = parse_network_address addr_recv in
      (
	match receiver_address with
	| None -> (None, rest)
	| Some receiver_address ->
	  (Some {
	    protocol_version = Int32.to_int version;
	    services = services_set_of_int64 services;
	    timestamp = Unix.localtime (Int64.to_float timestamp);
	    receiver_address = receiver_address;
	    sender_address = None;
	    random_nonce = None;
	    user_agent = None;
	    start_height = None;
	    relay = None;
	  }, rest)
      )
    | { _ } -> (None, bits)
  in

  let parse_version_message_v106 message bits =
    bitmatch bits with
    | { addr_from : 26*8 : bitstring;
	nonce : 8*8 : string;
	user_agent : ((bitstring_length bits) - (26+8+4 + (if message.protocol_version >= 70001 then 1 else 0))*8) : string; (* TODO: proper var_string parser *)
	start_height : 4*8 : littleendian;
	rest : -1 : bitstring
      } ->
      let sender_address = parse_network_address addr_from in
      (
	match sender_address with
	| None -> (None, rest)
	| Some sender_address ->
	  (Some { message with 
	    sender_address = Some sender_address;
	    random_nonce = Some nonce;
	    user_agent = Some (String.sub user_agent 1 ((String.length user_agent) - 1));
	    start_height = Some (Int32.to_int start_height);
          }, rest)
      )
    | { _ } -> (Some message, bits)
  in

  let parse_version_message_v70001 message bits = 
    bitmatch bits with
    | { relay : 1*8 : littleendian;
	rest : -1 : bitstring
      } -> (Some { message with relay = Some (relay > 0) }, rest)
    | { _ } -> (Some message, bits)
  in
  let message, rest = parse_version_message_v0 bits in
  match message with
  | None -> None
  | Some message when message.protocol_version >= 106 ->
    let message, rest = parse_version_message_v106 message rest in
    (
      match message with
      | None -> None
      | Some message when message.protocol_version >= 70001 -> 
	let message, rest = parse_version_message_v70001 message rest in
	(
	  match message with
	  | None -> None
	  | Some message -> Some (VersionPayload message)
	)
      | Some message -> Some (VersionPayload message)
    )
  | Some message -> Some (VersionPayload message)
;;

let parse_payload payload_string = function
  | VersionCommand -> parse_version_message (Bitstring.bitstring_of_string payload_string)
  | VerAckCommand -> Some VerAckPayload
  | _ -> Some (UnknownPayload payload_string)
;;

let read_string_from_fd fd bytes =
  let received_string = String.make bytes '\x00' in
  let bytes_read = Unix.read fd received_string 0 bytes in
  String.sub received_string 0 bytes_read
;;

let parse_header_from_fd fd =
  let bytestring = read_string_from_fd fd (4+12+4+4) in
  parse_header (Bitstring.bitstring_of_string bytestring)
;;

let read_payload_from_fd fd header =
  read_string_from_fd fd header.payload_length
;;
let verify_message_checksum header payload_string  =
  let payload_checksum = message_checksum payload_string in
  header.checksum = payload_checksum
;;

let read_and_parse_message_from_fd fd =
  let header = parse_header_from_fd fd in
  let payload_string = read_payload_from_fd fd header in
  if not (verify_message_checksum header payload_string) then None
  else
    let payload = parse_payload payload_string header.command in
    match payload with
    | None -> None
    | Some payload -> Some { network = header.magic; payload = payload }
;;
