open Bitcoin_protocol;;
open Bitstring;;

let default_random_nonce = "\x00\x00\xde\xad\xbe\xef\x00\x00";;

let bitstring_of_header header =
  BITSTRING {
    int32_of_magic header.magic : 4*8 : littleendian;
    Utils.zeropad_string_to_length (string_of_command header.command) 12 : 12*8 : string;
    Int32.of_int header.payload_length : 4*8 : littleendian;
    header.checksum : 4*8 : string
  }
;;

let bitstring_of_network_address network_address =
  BITSTRING {
    int64_of_services_set network_address.services : 8*8 : littleendian;
    network_address.address : 16*8 : string;
    network_address.port : 2*8 : bigendian
  }
;;

let varint_bitstring_of_int64 = function
  | i when i < 0xfdL -> BITSTRING { Int64.to_int i : 1*8 : littleendian }
  | i when i < 0xffffL -> BITSTRING { Int64.to_int i : 2*8 : littleendian }
  | i when i < 0xffffffffL -> BITSTRING { Int64.to_int32 i : 4*8 : littleendian }
  | i -> BITSTRING { i : 8*8 : littleendian }
;;

let varstring_bitstring_of_string s = 
  let length_varint_bitstring = varint_bitstring_of_int64 (Int64.of_int (String.length s)) in
  BITSTRING {
    length_varint_bitstring : -1 : bitstring;
    s : (String.length s) * 8 : string
  }
;;

let bitstring_of_version_message m =
  let message_bitstring = BITSTRING {
    Int32.of_int m.protocol_version : 4*8 : littleendian;
    int64_of_services_set m.node_services : 8*8 : littleendian;
    Int64.of_float (fst (Unix.mktime m.timestamp)) : 8*8 : littleendian;
    bitstring_of_network_address m.receiver_address : 26*8 : bitstring
  } in
  let message_bitstring = if m.protocol_version < 106 then message_bitstring else
      BITSTRING {
	message_bitstring : -1 : bitstring;
	bitstring_of_network_address (Option.default { services = ServiceSet.empty; address = String.make 16 '\x00'; port = 0 } m.sender_address) : 26*8 : bitstring;
	Option.default default_random_nonce m.random_nonce : 8*8 : string;
	varstring_bitstring_of_string (Option.default "" m.user_agent) : -1 : bitstring;
	Int32.of_int (Option.default 0 m.start_height) : 4*8 : littleendian
      }
  in
  let message_bitstring = if m.protocol_version < 70001 then message_bitstring else
      BITSTRING {
	message_bitstring : -1 : bitstring;
	Utils.int_of_bool (Option.default false m.relay) : 1*8 : littleendian
      }
  in
  message_bitstring
;;

let bitstring_of_verack_message () = empty_bitstring;;

let bitstring_of_addr_message m =
  let rec bitstrings_of_addresses = function
    | [] -> []
    | { address_timestamp = timestamp; 
	network_address = network_address;
      } :: addresses -> 
      (* using a 0 timestamp as default is a bit of a hack, really, bit it should achieve the same result: this address will not be used by the receiving node *)      
      let address_bitstring = BITSTRING {
	Utils.int64_of_unix_tm (Option.default (Unix.gmtime 0.0) timestamp) : 8*8 : littleendian;
	bitstring_of_network_address network_address : -1 : bitstring
      } in
      address_bitstring :: bitstrings_of_addresses addresses
  in
  let address_count_varint_bitstring = varint_bitstring_of_int64 (Int64.of_int (List.length m.addresses)) in
  BITSTRING {
    address_count_varint_bitstring : -1 : bitstring;
    concat (bitstrings_of_addresses m.addresses) : -1 : bitstring
  }
;;

let bitstring_of_getaddr_message () = empty_bitstring;;

let bitstring_of_inventory_item item =
  BITSTRING {
    int32_of_inventory_item_type item.inventory_item_type : 4*8 : littleendian;
    item.inventory_item_hash : 32*8 : string
  }
;;
let bitstring_of_inventory_list_message m =
  let rec bitstrings_of_inventory_items = function
    | [] -> []
    | item :: items -> 
      let item_bitstring = bitstring_of_inventory_item item in
      item_bitstring :: bitstrings_of_inventory_items items
  in
  let item_count_varint_bitstring = varint_bitstring_of_int64 (Int64.of_int (List.length m.inventory)) in
  BITSTRING {
    item_count_varint_bitstring : -1 : bitstring;
    concat (bitstrings_of_inventory_items m.inventory) : -1 : bitstring
  }
;;
let bitstring_of_inv_message m = bitstring_of_inventory_list_message m;;
let bitstring_of_getdata_message m = bitstring_of_inventory_list_message m;;
let bitstring_of_notfound_message m = bitstring_of_inventory_list_message m;;

let bitstring_of_block_locator_list_message m =
  let rec bitstrings_of_block_locators = function
    | [] -> []
    | hash :: hashes ->
      (BITSTRING { hash : 32*8 : string }) :: bitstrings_of_block_locators hashes
  in
  let block_locator_count_varint_bitstring = varint_bitstring_of_int64 (Int64.of_int (List.length m.block_locator_hashes)) in
  BITSTRING {
    Int32.of_int m.block_protocol_version : 4*8 : littleendian;
    block_locator_count_varint_bitstring : -1 : bitstring;
    concat (bitstrings_of_block_locators m.block_locator_hashes) : -1 : bitstring;
    m.block_locator_hash_stop : 32*8 : string
  }
;;
let bitstring_of_getblocks_message m = bitstring_of_block_locator_list_message m;;
let bitstring_of_getheaders_message m = bitstring_of_block_locator_list_message m;;

let bitstring_of_payload = function
  | VersionPayload m -> bitstring_of_version_message m
  | VerAckPayload -> bitstring_of_verack_message ()
  | AddrPayload m -> bitstring_of_addr_message m
  | GetAddrPayload -> bitstring_of_getaddr_message ()
  | InvPayload m -> bitstring_of_inv_message m
  | GetDataPayload m -> bitstring_of_getdata_message m
  | NotFoundPayload m -> bitstring_of_notfound_message m
  | GetBlocksPayload m -> bitstring_of_getblocks_message m
  | GetHeadersPayload m -> bitstring_of_getheaders_message m
  | UnknownPayload bs -> bs
;;

let bitstring_of_message m = 
  let payload_bitstring = bitstring_of_payload m.payload in
  let header = {
    magic = m.network;
    command = command_of_message_payload m.payload;
    payload_length = (bitstring_length payload_bitstring) / 8;
    checksum = message_checksum (string_of_bitstring payload_bitstring);
  } in
  let header_bitstring = bitstring_of_header header in
  BITSTRING {
    header_bitstring : -1 : bitstring;
    payload_bitstring : -1 : bitstring
  }
;;










