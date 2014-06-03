open Bitcoin_protocol;;

let pp_string_of_magic = function
  | MainNetwork -> "Main Network"
  | TestNet -> "Testnet"
  | TestNet3 -> "Testnet 3"
  | UnknownMagic i -> Printf.sprintf "Unknown (%lu)" i
;;

let pp_string_of_command = function
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
  | UnknownCommand s -> Printf.sprintf "Unknown (%s)" s
;;

let pp_string_of_services_set set =
  let pp_string_of_service = function
    | NetworkNodeService -> "Full Network Node"
  in
  if ServiceSet.is_empty set then "No Services"
  else String.concat ", " (List.map pp_string_of_service (ServiceSet.elements set))
;;

let pp_string_of_ipv4_address address_string =
  Printf.sprintf "%d.%d.%d.%d"
    (Char.code (address_string.[String.length address_string - 4]))
    (Char.code (address_string.[String.length address_string - 3]))
    (Char.code (address_string.[String.length address_string - 2]))
    (Char.code (address_string.[String.length address_string - 1]))
;;

let pp_string_of_network_address n =
  Printf.sprintf "%s:%d (%s)" (pp_string_of_ipv4_address n.address) n.port (pp_string_of_services_set n.services)
;;

let print_header header = 
  print_endline "Bitcoin Message Header:";
  Printf.printf "\tMagic: %s\n" (pp_string_of_magic header.magic);
  Printf.printf "\tCommand: %s\n" (pp_string_of_command header.command);
  Printf.printf "\tPayload Size: %d bytes\n" header.payload_length;
  print_string "\tChecksum: "; Utils.print_hex_string header.checksum 0; print_newline ();
;;

let print_version_message m = 
  print_endline "Bitcoin Version Message:";
  Printf.printf "\tProtocol Version: %d\n" m.protocol_version;
  Printf.printf "\tServices: %s\n" (pp_string_of_services_set m.node_services);
  Printf.printf "\tTimestamp: %s\n" (Utils.string_of_unix_tm m.timestamp);
  Printf.printf "\tReceiver: %s\n" (pp_string_of_network_address m.receiver_address);
  Option.may (fun sender_address -> Printf.printf "\tSender: %s\n" (pp_string_of_network_address sender_address)) m.sender_address;
  Option.may (fun random_nonce -> Printf.printf "\tRandom Nonce: 0x%08Lx\n" random_nonce) m.random_nonce;
  Option.may (fun user_agent -> Printf.printf "\tUser Agent: %s\n" user_agent) m.user_agent;
  Option.may (fun start_height -> Printf.printf "\tStarting block height: %d\n" start_height) m.start_height;
  Option.may (fun relay -> Printf.printf "\tRelay transactions?: %s\n" (if relay then "Yes" else "No")) m.relay
;;

let print_verack_message () =
  print_endline "Bitcoin Version Acknowledgement Message"
;;

let print_addr_message m =
  let rec print_addresses index = function
    | [] -> ()
    | address :: addresses ->
      Printf.printf "\t%d:\t%s\n" index (pp_string_of_network_address address.network_address);
      Option.may (fun timestamp -> Printf.printf "\t\t(Last seen: %s)\n" (Utils.string_of_unix_tm timestamp)) address.address_timestamp;
      print_addresses (index+1) addresses
  in
  Printf.printf "Bitcoin Address Message (%d addresses):\n" (List.length m.addresses);
  print_addresses 1 m.addresses
;;

let print_getaddr_message () =
  print_endline "Bitcoin Get Addresses Message"
;;

let pp_string_of_inventory_item_type = function
  | TransactionInventoryItem -> "Transaction"
  | BlockInventoryItem -> "Block"
  | UnknownInventoryItem i -> Printf.sprintf "Unknown (%d)" i
;;

let pp_string_of_inventory_item item =
  Printf.sprintf "%s: %s" (pp_string_of_inventory_item_type item.inventory_item_type) (Utils.hex_string_of_hash_string item.inventory_item_hash)
;;

let print_inventory_list_message m =
  let rec print_inventory_list index = function
    | [] -> ()
    | item :: items ->
      Printf.printf "\t%d:\t%s\n" index (pp_string_of_inventory_item item);
      print_inventory_list (index+1) items
  in
  print_inventory_list 1 m.inventory
;;

let print_inventory_list_message_with_header m message_type =
  Printf.printf "Bitcoin %s Message:\n" message_type;
  print_inventory_list_message m
;;

let print_block_locator_list_message m =
  let rec print_block_locator index = function
    | [] -> ()
    | hash :: hashes ->
      Printf.printf "\t%d:\t%s\n" index (Utils.hex_string_of_hash_string hash);
      print_block_locator (index+1) hashes
  in
  Printf.printf "\tBlock protocol version: %d\n" m.block_protocol_version;
  Printf.printf "\tBlock Locator (%d entries):\n" (List.length m.block_locator_hashes);
  print_block_locator 1 m.block_locator_hashes;
  Printf.printf "\tLast desired Block: %s\n" (if m.block_locator_hash_stop = (String.make 32 '\x00') then "None" else Utils.hex_string_of_hash_string m.block_locator_hash_stop)
;;
let print_block_locator_list_message_with_header m message_type =
  Printf.printf "Bitcoin %s Message:\n" message_type;
  print_block_locator_list_message m
;;

let pp_string_of_transaction_lock_time = function
  | AlwaysLockedTransaction -> "Always locked"
  | BlockLockedTransaction i -> Printf.sprintf "block height %lu" i
  | TimestampLockedTransaction timestamp -> Utils.string_of_unix_tm timestamp
;;

let pp_string_of_transaction_outpoint outpoint =
  Printf.sprintf "%s #%d" (Utils.hex_string_of_hash_string outpoint.referenced_transaction_hash) outpoint.transaction_output_index
;;

let pp_string_of_output_value value =
  Printf.sprintf "%f BTC" ((Int64.to_float value) /. satoshis_per_bitoin)
;;

let print_transaction transaction =
  let rec print_transaction_inputs index = function
    | [] -> ()
    | input :: inputs ->
      Printf.printf "\t%d:\tReferenced output: %s\n" index (pp_string_of_transaction_outpoint input.previous_transaction_output);
      Printf.printf "\t\tSequence Number: %s\n" (if input.transaction_sequence_number = 0xffffffffl then "Final" else Printf.sprintf "0x%lx" input.transaction_sequence_number);
      Printf.printf "\t\tSignature script:\n"; Utils.print_indented_hex_string input.signature_script 0 2;
      print_newline ();
      print_transaction_inputs (index + 1) inputs
  in
  let rec print_transaction_outputs index = function
    | [] -> ()
    | output :: outputs ->
      Printf.printf "\t%d:\tValue: %s\n" index (pp_string_of_output_value output.transaction_output_value);
      Printf.printf "\t\tOutput script:\n"; Utils.print_indented_hex_string output.output_script 0 2;
      print_newline ();
      print_transaction_outputs (index + 1) outputs
  in
  Printf.printf "\tData Format Version: %d\n" transaction.transaction_data_format_version;
  Printf.printf "\tInputs (%d entries)\n" (List.length transaction.transaction_inputs);
  print_transaction_inputs 1 transaction.transaction_inputs;
  Printf.printf "\tOutputs (%d entries)\n" (List.length transaction.transaction_outputs);
  print_transaction_outputs 1 transaction.transaction_outputs;
  Printf.printf "\tLocked until: %s\n" (pp_string_of_transaction_lock_time transaction.transaction_lock_time)
;;
let print_tx_message m =
  print_endline "Bitcoin Transaction Message:";
  print_transaction m
;;

let print_block_header header =
  Printf.printf "\tBlock Version: %u\n" header.block_version;
  Printf.printf "\tPrevious Block: %s\n" (Utils.hex_string_of_hash_string header.previous_block_hash);
  Printf.printf "\tMerkle Tree Root: %s\n" (Utils.hex_string_of_hash_string header.merkle_root);
  Printf.printf "\tCreated at: %s\n" (Utils.string_of_unix_tm header.block_timestamp);
  Printf.printf "\tDifficulty: %u\n" header.block_difficulty_target;
  Printf.printf "\tNonce: 0x%04lu\n" header.block_nonce;
;;
let print_protocol_block_header header =
  print_block_header header.basic_block_header;
  Printf.printf "\tTransactions: %Ld transactions\n" header.block_transaction_count;
;;

let print_block block =
  let print_transaction_with_index index transaction =
    Printf.printf "%d:\n" (index + 1);
    print_transaction transaction;
  in
  print_block_header block.block_header;
  Printf.printf "\tTransactions (%d entries):\n" (List.length block.block_transactions);
  List.iteri print_transaction_with_index block.block_transactions
;;
let print_block_message m =
  print_endline "Bitcoin Block Message:";
  print_block m
;;

let print_headers_message m =
  let print_protocol_block_header_with_index index protocol_block_header =
    Printf.printf "%d:\n" (index + 1);
    print_protocol_block_header protocol_block_header;
  in
  print_endline "Bitcoin Headers Message:";
  Printf.printf "\tHeaders (%d entries):\n" (List.length m.block_headers);
  List.iteri print_protocol_block_header_with_index m.block_headers
;;

let print_mempool_message () =
  print_endline "Bitcoin Memory Pool Message"
;;

let print_nonce_message m =
  Printf.printf "\tRandom Nonce: 0x%08Lx\n" m.message_nonce;
;;
let print_nonce_message_with_header m message_type =
  Printf.printf "Bitcoin %s Message:\n" message_type;
  print_nonce_message m
;;

let print_message_payload = function
  | VersionPayload p -> print_version_message p
  | VerAckPayload -> print_verack_message ()
  | AddrPayload p -> print_addr_message p
  | GetAddrPayload -> print_getaddr_message ()
  | InvPayload p -> print_inventory_list_message_with_header p "Inventory"
  | GetDataPayload p -> print_inventory_list_message_with_header p "Get Data"
  | NotFoundPayload p -> print_inventory_list_message_with_header p "Not Found"
  | GetBlocksPayload p -> print_block_locator_list_message_with_header p "Get Blocks"
  | GetHeadersPayload p -> print_block_locator_list_message_with_header p "Get Headers"
  | TxPayload p -> print_tx_message p
  | BlockPayload p -> print_block_message p
  | HeadersPayload p -> print_headers_message p
  | MemPoolPayload -> print_mempool_message ()
  | PingPayload p -> print_nonce_message_with_header p "Ping"
  | PongPayload p -> print_nonce_message_with_header p "Pong"
  | UnknownPayload s -> Printf.printf "Unknown Message Payload (%d bytes)\n" (Bitstring.bitstring_length s)
;;

let print_message m =
  Printf.printf "Network: %s\n" (pp_string_of_magic m.network);
  print_message_payload m.payload;
;;
