let test_version_message () =
  let services_set = Bitcoin.Protocol.ServiceSet.add Bitcoin.Protocol.NetworkNodeService Bitcoin.Protocol.ServiceSet.empty in
  let localhost_address_string = (String.make 10 '\x00') ^ "\xff\xff" ^ "\127\000\000\001" in
  let receiver_address = {
    Bitcoin.Protocol.services = services_set;
    address = localhost_address_string;
    port = Config.peer_port;
  } in
  let sender_address = receiver_address in
  let random_nonce = Utils.le_bytestring_of_int64 (Random.int64 Int64.max_int) 8 in
  let payload = {
    Bitcoin.Protocol.protocol_version = Config.bitcoin_protocol_version;
    node_services = services_set;
    timestamp = Unix.localtime (Unix.time ());
    receiver_address = receiver_address;
    sender_address = Some sender_address;
    random_nonce = Some random_nonce;
    user_agent = Some Config.user_agent;
    start_height = Some 0;
    relay = Some true;
  } in
  {
    Bitcoin.Protocol.network = Bitcoin.Protocol.TestNet3;
    payload = Bitcoin.Protocol.VersionPayload payload;
  }
;;
let test_verack_message () =
  {
    Bitcoin.Protocol.network = Bitcoin.Protocol.TestNet3;
    payload = Bitcoin.Protocol.VerAckPayload;
  }
;;

let send_bitstring socket bitstring =
  let message_string = Bitstring.string_of_bitstring bitstring in
  Unix.write socket message_string 0 (String.length message_string)
;;
let send_message socket message =
  let message_bitstring = Bitcoin.Protocol.Generator.bitstring_of_message message in
  let bytes_written = send_bitstring socket message_bitstring in
  Printf.printf "Send message (%d bytes):\n" bytes_written;
  Bitcoin.Protocol.PP.print_message message;
  print_newline ();
;;

let receive_message protocol_version socket =
  let print_received_message_type m =
    Printf.printf "Received %s message\n" (Bitcoin.Protocol.PP.pp_string_of_command (Bitcoin.Protocol.command_of_message_payload m.Bitcoin.Protocol.payload))
  in
  let received_message = Bitcoin.Protocol.Parser.read_and_parse_message_from_fd protocol_version socket in
  match received_message with
  | None -> None
  | Some ({ Bitcoin.Protocol.network = Bitcoin.Protocol.TestNet3;
	   payload = Bitcoin.Protocol.TxPayload p;
	  } as tx_message) ->
    Bitcoin.Protocol.PP.print_message tx_message;
    received_message
  | received_message ->
    Option.may print_received_message_type received_message;
    received_message
;;
let rec receive_message_with_command command protocol_version socket =
  match receive_message protocol_version socket with
  | None -> None
  | Some m when (Bitcoin.Protocol.command_of_message_payload m.Bitcoin.Protocol.payload) = command -> Some m
  | Some m -> receive_message_with_command command protocol_version socket
;;

let send_version_message socket = send_message socket (test_version_message ());;
let send_verack_message socket = send_message socket (test_verack_message ());;

type connection_state =
| UninitializedState
| VersionSendState
| VersionReceivedState
| VerAckReceivedState
| VersionVerAckReceivedState
| InitializedState

let state_entry socket = function
  | UninitializedState ->
    print_endline "[FSM] Connection entered uninitialized state.";
    send_version_message socket;
    VersionSendState
  | VersionSendState ->
    print_endline "[FSM] Connection entered version_send state.";
    VersionSendState;
  | VersionReceivedState ->
    print_endline "[FSM] Connection entered version_received state.";
    VersionReceivedState
  | VerAckReceivedState ->
    print_endline "[FSM] Connection entered verack_received state.";
    VerAckReceivedState
  | VersionVerAckReceivedState ->
    print_endline "[FSM] Connection entered version_and_verack_received state.";
    send_verack_message socket;
    InitializedState
  | InitializedState ->
    print_endline "[FSM] Connection fully initialized.";
    InitializedState
;;

let transition state message = match (state, message.Bitcoin.Protocol.payload) with
  | VersionSendState, Bitcoin.Protocol.VersionPayload m -> VersionReceivedState
  | VersionSendState, Bitcoin.Protocol.VerAckPayload -> VerAckReceivedState
  | VersionReceivedState, Bitcoin.Protocol.VerAckPayload -> VersionVerAckReceivedState
  | VerAckReceivedState, Bitcoin.Protocol.VersionPayload m -> VersionVerAckReceivedState
  | state, _ -> state
;;

let handle_connection socket =
  let protocol_version = ref 0 in
  let rec connection_fsm state =
    let new_state = state_entry socket state in
    if new_state != state then connection_fsm new_state
    else
      match state with
      | InitializedState -> ()
      | state ->
	let received_message = receive_message !protocol_version socket in
	match received_message with
	| None -> connection_fsm state
	| Some { Bitcoin.Protocol.network = network; payload = Bitcoin.Protocol.VersionPayload p } as m ->
	  protocol_version := p.Bitcoin.Protocol.protocol_version;
	  connection_fsm (transition state (Option.get m))
	| Some m -> connection_fsm (transition state m)
  in
  connection_fsm UninitializedState;
  !protocol_version
;;

let test_getaddr_message socket =
  {
    Bitcoin.Protocol.network = Bitcoin.Protocol.TestNet3;
    payload = Bitcoin.Protocol.GetAddrPayload;
  }
;;
let send_getaddr_message socket = send_message socket (test_getaddr_message ());;

let exchange_addresses protocol_version socket =
  send_getaddr_message socket;
  let addresses_message = receive_message protocol_version socket in
  Option.default_f (fun () -> print_endline "No valid addresses received.") Bitcoin.Protocol.PP.print_message addresses_message
;;

let construct_block_locator_list_message protocol_version hash_stop known_block_hashes =
  let rec select_block_hashes step_size step count hashes = 
    let next_step_size = if count >= 10 then step_size*2 else step_size in
    match hashes, step with
    | [], _ -> []
    | hash :: [], _ -> [hash] (* always include the last hash (the genesis block hash) as an anchor *)
    | hash :: hashes, 0 -> hash :: (select_block_hashes next_step_size (next_step_size - 1) (count + 1) hashes)
    | hash :: hashes, x -> select_block_hashes step_size (step - 1) count hashes
  in
  let block_locator_hashes = select_block_hashes 1 0 0 known_block_hashes in
  {
    Bitcoin.Protocol.block_protocol_version = protocol_version;
    block_locator_hashes = block_locator_hashes;
    block_locator_hash_stop = hash_stop;
  }
;;

let rec download_block_chain protocol_version known_block_hashes socket =
  let rec block_hashes_of_inventory = function
    | [] -> []
    | { Bitcoin.Protocol.inventory_item_type = Bitcoin.Protocol.BlockInventoryItem;
	inventory_item_hash = hash; } :: inventory ->
      hash :: (block_hashes_of_inventory inventory)
    | item :: inventory -> block_hashes_of_inventory inventory
  in
  let get_blocks_message = {
    Bitcoin.Protocol.network = Bitcoin.Protocol.TestNet3;
    payload = Bitcoin.Protocol.GetBlocksPayload (construct_block_locator_list_message protocol_version (String.make 32 '\x00') known_block_hashes);
  } in
  send_message socket get_blocks_message;
  match receive_message_with_command Bitcoin.Protocol.InvCommand protocol_version socket with
  | None -> print_endline "No valid block hashes received."
  | Some { Bitcoin.Protocol.network = Bitcoin.Protocol.TestNet3;
	   payload = Bitcoin.Protocol.InvPayload inventory } ->
    let new_hashes = block_hashes_of_inventory inventory.Bitcoin.Protocol.inventory in
    let known_block_hashes = new_hashes @ known_block_hashes in
    Printf.printf "Received %d new block hashes, now have a total of %d block hashes.\n" (List.length new_hashes) (List.length known_block_hashes);
    download_block_chain protocol_version known_block_hashes socket   
  | Some m ->
    print_endline "Received unexpected message:";
    Bitcoin.Protocol.PP.print_message m
;;

let rec snoop_transactions protocol_version socket =
  let inventory_item_is_transaction item = item.Bitcoin.Protocol.inventory_item_type = Bitcoin.Protocol.TransactionInventoryItem in
  match receive_message_with_command Bitcoin.Protocol.InvCommand protocol_version socket with
  | None -> ()
  | Some ({ Bitcoin.Protocol.network = Bitcoin.Protocol.TestNet3;
	    payload = Bitcoin.Protocol.InvPayload inv_payload
	  } as inv_message) ->
    Bitcoin.Protocol.PP.print_message inv_message;
    ( match List.filter inventory_item_is_transaction inv_payload.Bitcoin.Protocol.inventory with
    | [] -> snoop_transactions protocol_version socket
    | transaction_inventory ->
      send_message socket { Bitcoin.Protocol.network = Bitcoin.Protocol.TestNet3;
			    payload = Bitcoin.Protocol.GetDataPayload { Bitcoin.Protocol.inventory = transaction_inventory; };
			  };
      snoop_transactions protocol_version socket
    )
  | _ -> ()
;;

(* main *)
let () =
  Random.self_init ();
  let client_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let peer_addr = Unix.ADDR_INET(Unix.inet_addr_of_string Config.peer_ip_address, Config.peer_port) in
  Unix.connect client_socket peer_addr;
  let peer_protocol_version = handle_connection client_socket in
  Printf.printf "Peer is running protocol version %d\n" peer_protocol_version;
  (* exchange_addresses peer_protocol_version client_socket; *)
  (* download_block_chain peer_protocol_version [Config.testnet3_genesis_block_hash] client_socket; *)
  snoop_transactions peer_protocol_version client_socket;
  Unix.sleep 1;
  Unix.close client_socket
;;
  

