open Bitcoin_protocol;;

let string_of_bitcoin_magic = function
  | MainNetwork -> "Main Network"
  | TestNet -> "Testnet"
  | TestNet3 -> "Testnet 3"
  | UnknownMagic i -> Printf.sprintf "Unknown (%d)" i
;;

let string_of_bitcoin_command = function
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

let string_of_bitcoin_services_set set =
  let string_of_bitcoin_service = function
    | NetworkNodeService -> "Full Network Node"
  in
  if BitcoinServiceSet.is_empty set then "No Services"
  else String.concat ", " (List.map string_of_bitcoin_service (BitcoinServiceSet.elements set))
;;

let string_of_bitcoin_ipv4_address address_string =
  Printf.sprintf "%d.%d.%d.%d"
    (Char.code (address_string.[String.length address_string - 4]))
    (Char.code (address_string.[String.length address_string - 3]))
    (Char.code (address_string.[String.length address_string - 2]))
    (Char.code (address_string.[String.length address_string - 1]))
;;

let string_of_bitcoin_network_address n =
  Printf.sprintf "%s:%d (%s)" (string_of_bitcoin_ipv4_address n.address) n.port (string_of_bitcoin_services_set n.services)
;;

let print_bitcoin_header header = 
  print_endline "Bitcoin Message Header:";
  Printf.printf "\tMagic: %s\n" (string_of_bitcoin_magic header.magic);
  Printf.printf "\tCommand: %s\n" (string_of_bitcoin_command header.command);
  Printf.printf "\tPayload Size: %d bytes\n" header.payload_length;
  print_string "\tChecksum: "; Utils.print_hex_string header.checksum 0; print_newline ();
;;

let print_bitcoin_version_message m = 
  print_endline "Bitcoin Version Message:";
  Printf.printf "\tProtocol Version: %d\n" m.protocol_version;
  Printf.printf "\tServices: %s\n" (string_of_bitcoin_services_set m.services);
  Printf.printf "\tTimestamp: %s\n" (Utils.string_of_unix_tm m.timestamp);
  Printf.printf "\tReceiver: %s\n" (string_of_bitcoin_network_address m.receiver_address);
  Option.may (fun sender_address -> Printf.printf "\tSender: %s\n" (string_of_bitcoin_network_address sender_address)) m.sender_address;
  Option.may (fun random_nonce -> print_string "\tRandom Nonce: "; Utils.print_hex_string random_nonce 0; print_newline()) m.random_nonce;
  Option.may (fun user_agent -> Printf.printf "\tUser Agent: %s\n" user_agent) m.user_agent;
  Option.may (fun start_height -> Printf.printf "\tStarting block height: %d\n" start_height) m.start_height;
  Option.may (fun relay -> Printf.printf "\tRelay transactions?: %s\n" (if relay then "No" else "Yes")) m.relay
;;

let print_bitcoin_message_payload = function
  | VersionPayload p -> print_bitcoin_version_message p
  | VerAckPayload -> ()
  | UnknownPayload s -> Printf.printf "Unknown Message Payload (%d bytes)\n" (String.length s)
;;

let print_bitcoin_message m =
  Printf.printf "Network: %s\n" (string_of_bitcoin_magic m.network);
  print_bitcoin_message_payload m.payload;
;;
