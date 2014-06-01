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
      magic = magic_of_int32 magic;
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
let parse_varint bits =
  let parse_tag_byte bits = 
    bitmatch bits with
    | { tag : 1*8 : littleendian;
	rest : -1 : bitstring
      } -> (Some tag, rest)
    | { _ } -> (None, bits)
  in
  let parse_value bits bytesize =
    bitmatch bits with
    | { value : bytesize*8 : littleendian;
	rest : -1 : bitstring
      } -> (Some value, rest)
    | { _ } -> (None, bits)
  in
  let tag, rest = parse_tag_byte bits in
  match tag with
  | None -> (None, rest)
  | Some 0xff -> parse_value rest 8
  | Some 0xfe -> parse_value rest 4
  | Some 0xfd -> parse_value rest 2
  | Some x -> (Some (Int64.of_int x), rest)
;;

(* we should support strings with the full length of MAX(int64) bytes, but due to bitstring requiring the length in BITS in an OCaml int (31 bits, signed), we can only support much much shorter strings *facepalm* *)
let parse_varstring bits =
  let length, bits = parse_varint bits in
  match length with
  | None -> (None, bits)
  | Some length ->
    bitmatch bits with
    | { value : (Int64.to_int length) * 8 : string;
	rest : -1 : bitstring
      } -> (Some value, rest)
    | { _ } -> (None, bits)
;;

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
	    node_services = services_set_of_int64 services;
	    timestamp = Utils.unix_tm_of_int64 timestamp;
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
	user_agent : ((bitstring_length bits) - (26+8+4 + (if message.protocol_version >= 70001 then 1 else 0))*8) : bitstring; (* we could properly parse a var_string at this position, but that would involve splitting this parser in two again. For version messages, we can determine the length of the var_string so we parse it out as a bitstring here and then extract the actual string below *)
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
	    user_agent = fst (parse_varstring user_agent);
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

let parse_verack_message bits = Some VerAckPayload;;

let parse_addr_message protocol_version bits =
  let rec parse_addresses timestamp_size count index bits =
    if index >= count then [] else
      bitmatch bits with
      | { timestamp : timestamp_size : littleendian;
	  network_address_struct : 26*8 : bitstring;
	  rest : -1 : bitstring
	} ->
	( match parse_network_address network_address_struct with
	| None -> []
	| Some network_address ->
	  {
	    address_timestamp = if timestamp_size = 0 then None else Some (Utils.unix_tm_of_int64 timestamp);
	    network_address = network_address;
	  } :: parse_addresses timestamp_size count (Int64.add index 1L) rest
	)
      | { _ } -> []
  in
  
  match parse_varint bits with
  | None, rest -> None
  | Some address_count, bits ->
    let timestamp_size = if protocol_version >= 31402 then 4*8 else 0 in
    Some (AddrPayload {
      addresses = parse_addresses timestamp_size address_count 0L bits;
    })
;;

let parse_getaddr_message bits = Some GetAddrPayload;;

let parse_inventory_list_message bits =
  let rec parse_inventory_list count index bits =
    if index >= count then [] else
      bitmatch bits with
      | { item_type : 4*8 : littleendian;
	  hash : 32*8 : string;
	  rest : -1 : bitstring
	} ->
	{
	  inventory_item_type = inventory_item_type_of_int32 item_type;
	  inventory_item_hash = hash;
	} :: parse_inventory_list count (Int64.add index 1L) rest
      | { _ } -> []
  in	
  match parse_varint bits with
  | None, rest -> None
  | Some inventory_count, bits ->
    Some {
      inventory = parse_inventory_list inventory_count 0L bits;
    }
;;
let parse_inv_message bits =
  match parse_inventory_list_message bits with
  | None -> None
  | Some inventory -> Some (InvPayload inventory)
;;
let parse_getdata_message bits =
  match parse_inventory_list_message bits with
  | None -> None
  | Some inventory -> Some (GetDataPayload inventory)
;;
let parse_notfound_message bits =
  match parse_inventory_list_message bits with
  | None -> None
  | Some inventory -> Some (NotFoundPayload inventory)
;;

let parse_payload protocol_version payload_bitstring = function
  | VersionCommand -> parse_version_message payload_bitstring
  | VerAckCommand -> parse_verack_message payload_bitstring
  | AddrCommand -> parse_addr_message protocol_version payload_bitstring
  | GetAddrCommand -> parse_getaddr_message payload_bitstring
  | InvCommand -> parse_inv_message payload_bitstring
  | GetDataCommand -> parse_getdata_message payload_bitstring
  | NotFoundCommand -> parse_notfound_message payload_bitstring
  | _ -> Some (UnknownPayload payload_bitstring)
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

let read_and_parse_message_from_fd protocol_version fd =
  let header = parse_header_from_fd fd in
  let payload_string = read_payload_from_fd fd header in
  if not (verify_message_checksum header payload_string) then None
  else
    let payload = parse_payload protocol_version (Bitstring.bitstring_of_string payload_string) header.command in
    match payload with
    | None -> None
    | Some payload -> Some { network = header.magic; payload = payload }
;;

let read_and_parse_message_from_string protocol_version s =
  let header = parse_header (Bitstring.bitstring_of_string (String.sub s 0 (4+12+4+4))) in
  let payload_string = String.sub s (4+12+4+4) header.payload_length in
  if not (verify_message_checksum header payload_string) then None
  else
    let payload = parse_payload protocol_version (Bitstring.bitstring_of_string payload_string) header.command in
    match payload with
    | None -> None
    | Some payload -> Some { network = header.magic; payload = payload }
;;
