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

let parse_block_locator_list_message bits =
  let rec parse_block_locator_list count index acc bits =
    if index >= count then (acc, bits) else
      bitmatch bits with
      | { block_locator_hash : 32*8 : string;
	  rest : -1 : bitstring
	} ->
	parse_block_locator_list count (Int64.add index 1L) (block_locator_hash :: acc) rest
      | { _ } -> (acc, bits)
  in
  let protocol_version, bits = bitmatch bits with
    | { version : 4*8 : littleendian;
	rest : -1 : bitstring
      } -> (Some version, rest)
    | { _ } -> (None, bits)
  in
  match protocol_version with
  | None -> None
  | Some protocol_version ->
    match parse_varint bits with
    | None, rest -> None
    | Some block_locator_count, bits ->
      let block_locator_list, bits = parse_block_locator_list block_locator_count 0L [] bits in
      if ((List.length block_locator_list) != (Int64.to_int block_locator_count)) then None
      else
	bitmatch bits with
	| { block_locator_hash_stop : 32*8 : string } ->
	  Some {
	    block_protocol_version = Int32.to_int protocol_version;
	    block_locator_hashes = List.rev block_locator_list;
	    block_locator_hash_stop = block_locator_hash_stop;
	  }
	| { _ } -> None
;;
let parse_getblocks_message bits =
  match parse_block_locator_list_message bits with
  | None -> None
  | Some block_locator_list_message -> Some (GetBlocksPayload block_locator_list_message)
;;
let parse_getheaders_message bits =
  match parse_block_locator_list_message bits with
  | None -> None
  | Some block_locator_list_message -> Some (GetHeadersPayload block_locator_list_message)
;;

let parse_transaction bits =
  let rec parse_inputs count index acc bits = 
    if index >= count then (acc, bits) else
      bitmatch bits with
      | { outpoint_hash : 32*8 : string;
	  outpoint_index : 4*8 : littleendian;
	  rest : -1 : bitstring
	} ->
	( match parse_varstring rest with
	| None, bits -> (acc, bits)
	| Some signature_script, bits ->
	  bitmatch bits with
	  | { sequence_number : 4*8 : littleendian;
	      rest : -1 : bitstring
	    } ->
	    let input = {
	      previous_transaction_output = { referenced_transaction_hash = outpoint_hash;
					      transaction_output_index = Int32.to_int outpoint_index;
					    };
	      signature_script = signature_script;
	      transaction_sequence_number = sequence_number;
	    } in 
	    parse_inputs count (Int64.add index 1L) (input :: acc) rest
	  | { _ } -> (acc, bits)
	)
      | { _ } -> (acc, bits)
  in
  let rec parse_outputs count index acc bits =
    if index >= count then (acc, bits) else
      bitmatch bits with
      | { value : 8*8 : littleendian;
	  rest : -1 : bitstring
	} -> 
	match parse_varstring rest with
	| None, bits -> (acc, bits)
	| Some output_script, rest ->
	  let output = {
	    transaction_output_value = value;
	    output_script = output_script;
	  } in
	  parse_outputs count (Int64.add index 1L) (output :: acc) rest
  in
  let data_format_version, bits = bitmatch bits with
    | { version : 4*8 : littleendian;
	rest : -1 : bitstring
      } -> (Some version, rest)
    | { _ } -> (None, bits)
  in
  match data_format_version with
  | None -> (None, bits)
  | Some data_format_version ->
    match parse_varint bits with
    | None, rest -> (None, rest)
    | Some input_count, bits ->
      let inputs, bits = parse_inputs input_count 0L [] bits in
      match parse_varint bits with
      | None, rest -> (None, rest)
      | Some output_count, bits ->
	let outputs, bits = parse_outputs output_count 0L [] bits in
	if
	  ((List.length inputs) != (Int64.to_int input_count)) ||
	  ((List.length outputs) != (Int64.to_int output_count))
	then (None, bits)
	else
	  bitmatch bits with
	  | { lock_time : 4*8 : littleendian;
	      rest : -1 : bitstring } ->
	    (Some {
	      transaction_data_format_version = Int32.to_int data_format_version;
	      transaction_inputs = List.rev inputs;
	      transaction_outputs = List.rev outputs;
	      transaction_lock_time = transaction_lock_time_of_int32 lock_time;
	    }, rest)
	  | { _ } -> (None, bits)
;;
let parse_tx_message bits =
  match parse_transaction bits with
  | None, _ -> None
  | Some transaction, _ -> Some (TxPayload transaction)
;;

let parse_block_header bits = 
  bitmatch bits with
  | { version : 4*8 : littleendian;
      previous_block_hash : 32*8 : string;
      merkle_root : 32*8 : string;
      timestamp : 4*8 : littleendian;
      difficulty_target : 4*8 : littleendian;
      nonce : 4*8 : littleendian;
      rest : -1 : bitstring
    } ->
    (Some {
      block_version = Int32.to_int version;
      previous_block_hash = previous_block_hash;
      merkle_root = merkle_root;
      block_timestamp = (Utils.unix_tm_of_int32 timestamp);
      block_difficulty_target = Int32.to_int difficulty_target;
      block_nonce = nonce;
    }, rest)
  | { _ } -> (None, bits)
;;
let parse_protocol_block_header bits =
  match parse_block_header bits with
  | None, rest -> (None, rest)
  | Some block_header, bits ->
    match parse_varint bits with
    | None, rest -> (None, rest)
    | Some transaction_count, bits ->
      (Some {
	basic_block_header = block_header;
	block_transaction_count = transaction_count;
      }, bits)
;;    
let parse_block bits =
  let rec parse_transaction_list count index acc bits =
    if index >= count then (acc, bits) else
      match parse_transaction bits with
      | None, rest -> (acc, rest)
      | Some transaction, bits ->
	parse_transaction_list count (Int64.add index 1L) (transaction :: acc) bits
  in
  match parse_protocol_block_header bits with
  | None, rest -> (None, rest)
  | Some protocol_block_header, bits ->
    let transaction_count = protocol_block_header.block_transaction_count in
    let transactions, bits = parse_transaction_list transaction_count 0L [] bits in
    if (List.length transactions != (Int64.to_int transaction_count)) then (None, bits)
    else
      (Some {
	block_header = protocol_block_header.basic_block_header;
	block_transactions = List.rev transactions;
      }, bits)
;;
let parse_block_message bits =
  match parse_block bits with
  | None, _ -> None
  | Some block, _ -> Some (BlockPayload block)
;;

let parse_headers_message bits =
  let rec parse_protocol_block_header_list count index acc bits =
    if index >= count then (acc, bits) else
      match parse_protocol_block_header bits with
      | None, rest -> (acc, rest)
      | Some protocol_block_header, bits ->
	parse_protocol_block_header_list count (Int64.add index 1L) (protocol_block_header :: acc) bits
  in
  match parse_varint bits with
  | None, rest -> None
  | Some block_header_count, bits ->
    let protocol_block_headers, bits = parse_protocol_block_header_list block_header_count 0L [] bits in
    if (List.length protocol_block_headers != (Int64.to_int block_header_count)) then None
    else
      Some (HeadersPayload {
	block_headers = List.rev protocol_block_headers;
      })
;;

let parse_payload protocol_version payload_bitstring = function
  | VersionCommand -> parse_version_message payload_bitstring
  | VerAckCommand -> parse_verack_message payload_bitstring
  | AddrCommand -> parse_addr_message protocol_version payload_bitstring
  | GetAddrCommand -> parse_getaddr_message payload_bitstring
  | InvCommand -> parse_inv_message payload_bitstring
  | GetDataCommand -> parse_getdata_message payload_bitstring
  | NotFoundCommand -> parse_notfound_message payload_bitstring
  | GetBlocksCommand -> parse_getblocks_message payload_bitstring
  | GetHeadersCommand -> parse_getheaders_message payload_bitstring
  | TxCommand -> parse_tx_message payload_bitstring
  | BlockCommand -> parse_block_message payload_bitstring
  | HeadersCommand -> parse_headers_message payload_bitstring
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
