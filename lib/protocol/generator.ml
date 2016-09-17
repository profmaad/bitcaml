open Core.Std
open Bitstring
open Bitcoin_crypto.Std
open Bitcaml_utils.Std
open Types

let default_random_nonce = 0x0000deadbeef0000L;;

let bitstring_of_header (header : header) =
  [%bitstring
      {| int32_of_magic header.magic                                          :  4*8 : littleendian
       ; Utils.zeropad_string_to_length (string_of_command header.command) 12 : 12*8 : string
       ; Int.to_int32_exn header.payload_length                               :  4*8 : littleendian
       ; header.checksum                                                      :  4*8 : string
       |}
  ]
;;

let bitstring_of_network_address network_address =
  [%bitstring
      {| int64_of_services_set network_address.services :  8*8 : littleendian
       ; network_address.address                        : 16*8 : string
       ; network_address.port                           :  2*8 : bigendian
       |}
  ]
;;

let varint_bitstring_of_int64 = function
  | i when i < 0xfdL       -> [%bitstring {| Int.of_int64_exn i : 1*8 : littleendian |}]
  | i when i < 0xffffL     -> [%bitstring {| 0xfd : 1*8; Int64.to_int_exn i : 2*8 : littleendian |}]
  | i when i < 0xffffffffL -> [%bitstring {| 0xfe : 1*8; Int64.to_int32_exn i : 4*8 : littleendian |}]
  | i                      -> [%bitstring {| 0xff : 1*8; i : 8*8 : littleendian |}]
;;

let varstring_bitstring_of_string s =
  if (String.length s) = 0 then bitstring_of_string "\x00"
  else
    let length_varint_bitstring = varint_bitstring_of_int64 (Int64.of_int (String.length s)) in
    let length_length = bitstring_length length_varint_bitstring in
    [%bitstring
        {| length_varint_bitstring : length_length : bitstring;
         0x00 : 1 : littleendian
         |}
    ]
;;

let bitstring_of_version_message m =
  let message_bitstring =
    [%bitstring
        {| Int.to_int32_exn m.protocol_version                 :  4*8 : littleendian
         ; int64_of_services_set m.node_services           :  8*8 : littleendian
         ; Int64.of_float (fst (Unix.mktime m.timestamp))  :  8*8 : littleendian
         ; bitstring_of_network_address m.receiver_address : 26*8 : bitstring
         |}
    ]
  in
  let message_bitstring =
    if m.protocol_version < 106 then message_bitstring else
      let network_address =
        bitstring_of_network_address
          (Option.value ~default:{ services = Service.Set.empty; address = String.make 16 '\x00'; port = 0 } m.sender_address)
      in
      let random_nonce = Option.value ~default:default_random_nonce m.random_nonce in
      let user_agent = varstring_bitstring_of_string (Option.value ~default:"" m.user_agent) in
      let start_height = Int.to_int32_exn (Option.value ~default:0 m.start_height) in
      [%bitstring
          {| message_bitstring : -1 : bitstring
	   ; network_address : 26*8 : bitstring
	   ; random_nonce : 8*8 : littleendian
	   ; user_agent : -1 : bitstring
	   ; start_height : 4*8 : littleendian
           |}
      ]
  in
  let message_bitstring =
    if m.protocol_version < 70001 then message_bitstring else
      let relay = Utils.int_of_bool (Option.value ~default:false m.relay) in
      [%bitstring
          {| message_bitstring : -1 : bitstring
	   ; relay : 1*8 : littleendian
           |}
      ]
  in
  message_bitstring
;;

let bitstring_of_verack_message () = empty_bitstring;;

(* using a 0 timestamp as default is a bit of a hack, really, bit it should achieve the same result: this address will not be used by the receiving node *)
let bitstring_of_timestamped_network_address address =
  let timestamp = Utils.int64_of_unix_tm (Option.value ~default:(Unix.gmtime 0.0) address.address_timestamp) in
  [%bitstring
      {| timestamp : 8*8 : littleendian
       ; bitstring_of_network_address address.network_address : -1 : bitstring
       |}
  ]
;;
let bitstring_of_addr_message m =
  let address_count_varint_bitstring = varint_bitstring_of_int64 (Int64.of_int (List.length m.addresses)) in
  let addresses = concat (List.map ~f:bitstring_of_timestamped_network_address m.addresses) in
  [%bitstring
      {| address_count_varint_bitstring : -1 : bitstring
       ; addresses : -1 : bitstring
       |}
  ]
;;

let bitstring_of_getaddr_message () = empty_bitstring;;

let bitstring_of_inventory_item item =
  [%bitstring
      {| int32_of_inventory_item_type item.inventory_item_type : 4*8 : littleendian;
       item.inventory_item_hash : 32*8 : string
       |}
  ]
;;
let bitstring_of_inventory_list_message m =
  let item_count_varint_bitstring = varint_bitstring_of_int64 (Int64.of_int (List.length m.inventory)) in
  let items = concat (List.map ~f:bitstring_of_inventory_item m.inventory) in
  [%bitstring
      {| item_count_varint_bitstring : -1 : bitstring
       ; items : -1 : bitstring
       |}
  ]
;;
let bitstring_of_inv_message m = bitstring_of_inventory_list_message m;;
let bitstring_of_getdata_message m = bitstring_of_inventory_list_message m;;
let bitstring_of_notfound_message m = bitstring_of_inventory_list_message m;;

let bitstring_of_block_locator_list_message m =
  let bitstring_of_hash hash = [%bitstring {| hash : 32*8 : string |}] in
  let block_locator_count_varint_bitstring = varint_bitstring_of_int64 (Int64.of_int (List.length m.block_locator_hashes)) in
  let block_locator_hashes = concat (List.map ~f:bitstring_of_hash m.block_locator_hashes) in
  [%bitstring
      {| Int.to_int32_exn m.block_protocol_version : 4*8 : littleendian
       ; block_locator_count_varint_bitstring : -1 : bitstring
       ; block_locator_hashes : -1 : bitstring
       ; m.block_locator_hash_stop : 32*8 : string
       |}
  ]
;;
let bitstring_of_getblocks_message m = bitstring_of_block_locator_list_message m;;
let bitstring_of_getheaders_message m = bitstring_of_block_locator_list_message m;;

let bitstring_of_transaction_input input =
  [%bitstring
      {| input.previous_transaction_output.referenced_transaction_hash : 32*8 : string;
       input.previous_transaction_output.transaction_output_index : 4*8 : littleendian;
       varstring_bitstring_of_string input.signature_script : -1 : bitstring;
       input.transaction_sequence_number : 4*8 : littleendian
       |}
  ]
;;
let bitstring_of_transaction_output output =
  [%bitstring
      {| output.transaction_output_value : 8*8 : littleendian;
       varstring_bitstring_of_string output.output_script : -1 : bitstring
       |}
  ]
;;
let bitstring_of_transaction t =
  let input_count_varint_bitstring = varint_bitstring_of_int64 (Int64.of_int (List.length t.transaction_inputs)) in
  let output_count_varint_bitstring = varint_bitstring_of_int64 (Int64.of_int (List.length t.transaction_outputs)) in
  let inputs = concat (List.map ~f:bitstring_of_transaction_input t.transaction_inputs) in
  let outputs = concat (List.map ~f:bitstring_of_transaction_output t.transaction_outputs) in
  [%bitstring
      {| Int.to_int32_exn t.transaction_data_format_version : 4*8 :littleendian
       ; input_count_varint_bitstring : -1 : bitstring
       ; inputs : -1 : bitstring
       ; output_count_varint_bitstring : -1 : bitstring
       ; outputs : -1 : bitstring
       ; int32_of_transaction_lock_time t.transaction_lock_time : 4*8 : littleendian
       |}
  ]
;;
let bitstring_of_transaction_message m = bitstring_of_transaction m;;

let bitstring_of_block_header header =
  [%bitstring
      {| Int.to_int32_exn header.block_version : 4*8 : littleendian;
       header.previous_block_hash : 32*8 : string;
       header.merkle_root : 32*8 : string;
       Utils.int32_of_unix_tm header.block_timestamp : 4*8 : littleendian;
       int32_of_difficulty_bits header.block_difficulty_target : 4*8 : littleendian;
       header.block_nonce : 4*8 : littleendian
       |}
  ]
;;
let bitstring_of_protocol_block_header header =
  [%bitstring
      {| bitstring_of_block_header header.basic_block_header : -1 : bitstring;
       varint_bitstring_of_int64 header.block_transaction_count : -1 : bitstring
       |}
  ]
;;
let bitstring_of_block block =
  let transaction_count_varint_bitstring = varint_bitstring_of_int64 (Int64.of_int (List.length block.block_transactions)) in
  let transactions = concat (List.map ~f:bitstring_of_transaction block.block_transactions) in
  [%bitstring
      {| bitstring_of_block_header block.block_header : -1 : bitstring
       ; transaction_count_varint_bitstring : -1 : bitstring
       ; transactions : -1 : bitstring
       |}
  ]
;;
let bitstring_of_block_message m = bitstring_of_block m;;

let bitstring_of_headers_message m =
  let headers_count_varint_bitstring = varint_bitstring_of_int64 (Int64.of_int (List.length m.block_headers)) in
  let headers = concat (List.map ~f:bitstring_of_protocol_block_header m.block_headers) in
  [%bitstring
      {| headers_count_varint_bitstring : -1 : bitstring
       ; headers : -1 : bitstring
       |}
  ]
;;

let bitstring_of_mempool_message () = empty_bitstring;;

let bitstring_of_nonce_message m =
  [%bitstring {| m.message_nonce : 8*8 : littleendian |}]
;;
let bitstring_of_ping_message m = bitstring_of_nonce_message m;;
let bitstring_of_pong_message m = bitstring_of_nonce_message m;;

let bitstring_of_reject_message m =
  [%bitstring
      {| varstring_bitstring_of_string m.rejected_message : -1 : bitstring;
       int_of_rejection_reason m.rejection_code : 1*8 : littleendian;
       varstring_bitstring_of_string m.rejection_reason : -1 : bitstring
       |}
  ]
;;

let bitstring_of_payload = function
  | VersionPayload m    -> bitstring_of_version_message m
  | VerAckPayload       -> bitstring_of_verack_message ()
  | AddrPayload m       -> bitstring_of_addr_message m
  | GetAddrPayload      -> bitstring_of_getaddr_message ()
  | InvPayload m        -> bitstring_of_inv_message m
  | GetDataPayload m    -> bitstring_of_getdata_message m
  | NotFoundPayload m   -> bitstring_of_notfound_message m
  | GetBlocksPayload m  -> bitstring_of_getblocks_message m
  | GetHeadersPayload m -> bitstring_of_getheaders_message m
  | TxPayload m         -> bitstring_of_transaction_message m
  | BlockPayload m      -> bitstring_of_block_message m
  | HeadersPayload m    -> bitstring_of_headers_message m
  | MemPoolPayload      -> bitstring_of_mempool_message ()
  | PingPayload m       -> bitstring_of_ping_message m
  | PongPayload m       -> bitstring_of_pong_message m
  | RejectPayload m     -> bitstring_of_reject_message m
  | AlertPayload bs     -> bs
  | UnknownPayload bs   -> bs
;;

let bitstring_of_message m =
  let payload_bitstring = bitstring_of_payload m.payload in
  let header = {
      magic = m.network;
      command = command_of_message_payload m.payload;
      payload_length = (bitstring_length payload_bitstring) / 8;
      checksum = Hashing.message_checksum (string_of_bitstring payload_bitstring);
    } in
  let header_bitstring = bitstring_of_header header in
  [%bitstring
      {| header_bitstring : -1 : bitstring;
       payload_bitstring : -1 : bitstring
       |}
  ]
;;

(* this ought to be somewhere else, but I can't find a good spot that doesn't result in circular dependency *d'oh* *)
let transaction_hash tx =
  let tx_bitstring = bitstring_of_transaction tx in
  Hashing.hash256 (Bitstring.string_of_bitstring tx_bitstring)
;;
let block_hash header =
  let header_bitstring = bitstring_of_block_header header in
  Hashing.double_sha256 (Bitstring.string_of_bitstring header_bitstring)
;;
