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
  Option.may (fun random_nonce -> print_string "\tRandom Nonce: "; Utils.print_hex_string random_nonce 0; print_newline()) m.random_nonce;
  Option.may (fun user_agent -> Printf.printf "\tUser Agent: %s\n" user_agent) m.user_agent;
  Option.may (fun start_height -> Printf.printf "\tStarting block height: %d\n" start_height) m.start_height;
  Option.may (fun relay -> Printf.printf "\tRelay transactions?: %s\n" (if relay then "No" else "Yes")) m.relay
;;

let print_message_payload = function
  | VersionPayload p -> print_version_message p
  | VerAckPayload -> ()
  | UnknownPayload s -> Printf.printf "Unknown Message Payload (%d bytes)\n" (String.length s)
;;

let print_message m =
  Printf.printf "Network: %s\n" (pp_string_of_magic m.network);
  print_message_payload m.payload;
;;
