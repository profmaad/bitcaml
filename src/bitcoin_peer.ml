open Bitcoin_protocol;;

type peer =
  {
    peer_network : magic;
    local_version : version_message;
    peer_version : version_message;
    peer_socket : Unix.file_descr;
    peer_debug : bool;
  };;

let default_version =
  let default_network_address = {
    services = ServiceSet.empty;
    address = (String.make 10 '\x00') ^ "\xff\xff" ^ "\127\000\000\001"; (* 127.0.0.1 *)
    port = 0;
  } in
  {
    protocol_version = 0;
    node_services = ServiceSet.empty;
    timestamp = Unix.localtime (Unix.time ());
    receiver_address = default_network_address;
    sender_address = None;
    random_nonce = None;
    user_agent = None;
    start_height = None;
    relay = None;
  };;

let debug_may peer f =
  if peer.peer_debug then (f ())
;;

let message_of_payload peer payload = 
  {
    network = peer.peer_network;
    payload = payload;
  }
;;

let send_bitstring peer bitstring =
  let message_string = Bitstring.string_of_bitstring bitstring in
  Unix.write peer.peer_socket message_string 0 (String.length message_string)
;;
let send_message peer message =
  let message_bitstring = Bitcoin_protocol_generator.bitstring_of_message message in
  let bytes_written = send_bitstring peer message_bitstring in
  debug_may peer (fun () -> Printf.printf "Send message (%d bytes):\n" bytes_written;
    Bitcoin_protocol_pp.print_message message;
    print_newline ();
  )
;;
let send_payload peer payload = send_message peer (message_of_payload peer payload);;

let receive_message peer =
  let print_received_message_type m =
    Printf.printf "Received %s message\n" (Bitcoin_protocol_pp.pp_string_of_command (command_of_message_payload m.payload))
  in
  let received_message = Bitcoin_protocol_parser.read_and_parse_message_from_fd peer.peer_version.protocol_version peer.peer_socket in
  debug_may peer (fun () -> Option.may print_received_message_type received_message);
  received_message
;;
let rec receive_message_with_command command peer =
  match receive_message peer with
  | None -> None
  | Some m when (command_of_message_payload m.payload) = command -> Some m
  | Some m -> receive_message_with_command command peer
;;

type connection_state =
| UninitializedState
| VersionSendState
| VersionReceivedState
| VerAckReceivedState
| VersionVerAckReceivedState
| InitializedState

let state_entry peer = function
  | UninitializedState ->
    debug_may peer (fun () -> print_endline "[FSM] Connection entered uninitialized state.");
    send_payload peer (VersionPayload peer.local_version);
    VersionSendState
  | VersionSendState ->
    debug_may peer (fun () -> print_endline "[FSM] Connection entered version_send state.");
    VersionSendState;
  | VersionReceivedState ->
    debug_may peer (fun () -> print_endline "[FSM] Connection entered version_received state.");
    VersionReceivedState
  | VerAckReceivedState ->
    debug_may peer (fun () -> print_endline "[FSM] Connection entered verack_received state.");
    VerAckReceivedState
  | VersionVerAckReceivedState ->
    debug_may peer (fun () -> print_endline "[FSM] Connection entered version_and_verack_received state.");
    send_payload peer VerAckPayload;
    InitializedState
  | InitializedState ->
    debug_may peer (fun () -> print_endline "[FSM] Connection fully initialized.");
    InitializedState
;;

let transition state message = match (state, message.payload) with
  | VersionSendState, VersionPayload m -> VersionReceivedState
  | VersionSendState, VerAckPayload -> VerAckReceivedState
  | VersionReceivedState, VerAckPayload -> VersionVerAckReceivedState
  | VerAckReceivedState, VersionPayload m -> VersionVerAckReceivedState
  | state, _ -> state
;;

let handle_connection peer =
  let peer_ref = ref peer in
  let rec connection_fsm state =
    let new_state = state_entry peer state in
    if new_state != state then connection_fsm new_state
    else
      match state with
      | InitializedState -> ()
      | state ->
	let received_message = receive_message !peer_ref in
	match received_message with
	| None -> connection_fsm state
	| Some ({ network = network; payload = VersionPayload p } as m) ->
	  peer_ref := { !peer_ref with peer_version = p };
	  connection_fsm (transition state m)
	| Some m -> connection_fsm (transition state m)
  in
  connection_fsm UninitializedState;
  !peer_ref
;;

let exchange_addresses peer =
  send_payload peer GetAddrPayload;
  let addresses_message = receive_message peer in
  Option.default_f (fun () -> print_endline "No valid addresses received.") Bitcoin_protocol_pp.print_message addresses_message
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
    block_protocol_version = protocol_version;
    block_locator_hashes = block_locator_hashes;
    block_locator_hash_stop = hash_stop;
  }
;;

let rec download_block_chain peer known_block_hashes =
  let rec block_hashes_of_inventory = function
    | [] -> []
    | { inventory_item_type = BlockInventoryItem;
	inventory_item_hash = hash; } :: inventory ->
      hash :: (block_hashes_of_inventory inventory)
    | item :: inventory -> block_hashes_of_inventory inventory
  in
  let get_blocks_payload = GetBlocksPayload (
    construct_block_locator_list_message peer.peer_version.protocol_version (String.make 32 '\x00') known_block_hashes
  ) in
  send_payload peer get_blocks_payload;
  match receive_message_with_command InvCommand peer with
  | None -> debug_may peer (fun () -> print_endline "No valid block hashes received.")
  | Some { network = network;
	   payload = InvPayload inventory } ->
    let new_hashes = block_hashes_of_inventory inventory.inventory in
    let known_block_hashes = new_hashes @ known_block_hashes in
    debug_may peer (fun () -> Printf.printf "Received %d new block hashes, now have a total of %d block hashes.\n" (List.length new_hashes) (List.length known_block_hashes));
    download_block_chain peer known_block_hashes
  | Some m ->
    debug_may peer (fun () ->
      print_endline "Received unexpected message:";
      Bitcoin_protocol_pp.print_message m
    )
;;

let rec snoop_transactions peer =
  let inventory_item_is_transaction item = item.inventory_item_type = TransactionInventoryItem in
  match receive_message_with_command InvCommand peer with
  | None -> ()
  | Some ({ network = network;
	    payload = InvPayload inv_payload
	  } as inv_message) ->
    debug_may peer (fun () -> Bitcoin_protocol_pp.print_message inv_message);
    ( match List.filter inventory_item_is_transaction inv_payload.inventory with
    | [] -> snoop_transactions peer
    | transaction_inventory ->
      send_payload peer (GetDataPayload { inventory = transaction_inventory; });
      snoop_transactions peer
    )
  | _ -> ()
;;

let get_block peer block_hash =
  let payload = GetDataPayload {
    inventory = [
      {
	inventory_item_type = BlockInventoryItem;
	inventory_item_hash = block_hash;
      }
    ];
  } in
  send_payload peer payload;
  match receive_message_with_command BlockCommand peer with
  | None -> debug_may peer (fun () -> print_endline "No valid block received.")
  | Some m -> debug_may peer (fun () -> Bitcoin_protocol_pp.print_message m)
;;

let test_connection peer =
  let ping_nonce = Random.int64 Int64.max_int in
  let ping = PingPayload { message_nonce = ping_nonce; } in
  send_payload peer ping;
  match receive_message_with_command PongCommand peer with
  | None ->
    debug_may peer (fun () -> print_endline "No valid pong received.");
    false
  | Some ({
    network = network;
    payload = PongPayload { message_nonce = pong_nonce; };
  } as pong) ->
    debug_may peer (fun () -> Bitcoin_protocol_pp.print_message pong);
    if pong_nonce = ping_nonce then (
      debug_may peer (fun () -> print_endline "Ping Pong all day long!");
      true
    ) else (
      debug_may peer (fun () -> print_endline "I pinged, but the pong wasn't what I expected!");
      false;
    )
  | Some x ->
    debug_may peer (fun () -> print_endline "No valid pong received.");
    false
;;
