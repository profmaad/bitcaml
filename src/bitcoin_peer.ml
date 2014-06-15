open Bitcoin_protocol;;

type peer =
  {
    peer_network : magic;
    local_version : version_message;
    peer_version : version_message;
    peer_socket : Unix.file_descr;
    peer_debug : bool;
    blockchain : Bitcoin_blockchain.t;
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

let send_rejection peer message code reason =
  send_payload peer (RejectPayload {
    rejected_message = message;
    rejection_code = code;
    rejection_reason = reason;
  })
;;

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

(* TODO: properly handle network time *)
let network_median_time _ = 
  Utils.unix_tm_of_now ()
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

let initialize_connection peer =
  let peer_ref = ref peer in
  let rec connection_fsm state =
    let new_state = state_entry peer state in
    if new_state <> state then connection_fsm new_state
    else
      match state with
      | InitializedState -> ()
      | state ->
	let received_message = receive_message !peer_ref in
	match received_message with
	| None -> connection_fsm state
	| Some ({ network = network; payload = VersionPayload p } as m) when network = peer.peer_network ->
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
  match addresses_message with
  | Some ({ network = network;
	    payload = AddrPayload addresses_message } as m) when network = peer.peer_network ->
    debug_may peer (fun () -> Bitcoin_protocol_pp.print_message m);
    addresses_message.addresses
  | _ ->
    debug_may peer (fun () -> print_endline "No valid addresses received.");
    []
;;

let construct_block_locator_list_message peer hash_stop =
  let rec select_block_hashes step_size height count blockchain_db = 
    let next_step_size = if count >= 10L then Int64.mul step_size 2L else step_size in
    let height = if height <= 0L then 0L else height in
    match Bitcoin_blockchain_db.retrieve_block_at_height height blockchain_db with
    | None -> []
    | Some (_, hash, _, _, _, _, _, _, _, _, _) ->
      hash :: if height = 0L then [] else
	  (select_block_hashes next_step_size (Int64.sub height next_step_size) (Int64.add count 1L) blockchain_db)
  in
  let mainchain_height = Bitcoin_blockchain_db.mainchain_height peer.blockchain.Bitcoin_blockchain.db in
  (* debug_may peer (fun () -> Printf.printf "[INFO] current mainchain height: %Lu\n" mainchain_height); *)
  Printf.printf "[INFO] current mainchain height: %Lu\n" mainchain_height; flush stdout;
  let block_locator_hashes = select_block_hashes 1L mainchain_height 0L peer.blockchain.Bitcoin_blockchain.db in
  {
    block_protocol_version = peer.peer_version.protocol_version;
    block_locator_hashes = block_locator_hashes;
    block_locator_hash_stop = hash_stop;
  }
;;

let initiate_blockchain_download peer =
  send_payload peer (GetBlocksPayload (construct_block_locator_list_message peer (Utils.zero_hash)))
;;

let block_inventory_of_inventory inventory =
  List.filter (fun item -> item.inventory_item_type = BlockInventoryItem) inventory
;;
let block_hashes_of_inventory p = 
  List.map (fun item -> item.inventory_item_hash) (block_inventory_of_inventory p.inventory)
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
  | None ->
    debug_may peer (fun () -> print_endline "No valid block received.");
    None
  | Some ({ network = network;
	    payload = BlockPayload block;
	  } as m) when network = peer.peer_network ->
    debug_may peer (fun () -> Bitcoin_protocol_pp.print_message m);
    Some block
  | _ -> None
;;

let answer_ping peer ping =
  let pong = PongPayload { message_nonce = ping.message_nonce; } in
  send_payload peer pong
;;

let own_addr_payload peer =
  AddrPayload {
    addresses = [
      {
	address_timestamp = Some (Utils.unix_tm_of_now ());
	network_address = Option.get peer.local_version.sender_address;
      }
    ];
  }
;;

let handle_block peer block =
  let hash = Bitcoin_protocol_generator.block_hash block.block_header in
  debug_may peer (fun () -> Printf.printf "[INFO] received block %s\n%!" (Utils.hex_string_of_hash_string hash));  
  (try Bitcoin_blockchain.handle_block peer.blockchain (network_median_time peer) block with
  | Bitcoin_blockchain.BlockIsOrphan ->
    debug_may peer (fun () -> Printf.printf "[INFO] inserted block %s as orphan\n" (Utils.hex_string_of_hash_string hash));
    send_payload peer (GetBlocksPayload (construct_block_locator_list_message peer block.block_header.previous_block_hash))
  | Bitcoin_blockchain.BlockIsDuplicate ->
    debug_may peer (fun () -> Printf.printf "[INFO] block %s is a duplicate, not inserted\n" (Utils.hex_string_of_hash_string hash));
  | Bitcoin_blockchain.Rejected (reason, rejection_code) ->
    debug_may peer (fun () -> Printf.printf "[INFO] rejecting block %s: %s\n" (Utils.hex_string_of_hash_string hash) reason);
    (* send_rejection peer "block" rejection_code reason; *)
    ignore (exit 23);
  );
;;

let handle_inv_payload peer p =
  let block_inventory = block_inventory_of_inventory p.inventory in
  if (List.length block_inventory) > 0 then (
    debug_may peer (fun () -> Printf.printf "[INFO] sending getdata for %d blocks\n" (List.length block_inventory));
    send_payload peer (GetDataPayload { inventory = block_inventory; })
  )
;;


let handle_getdata_payload peer p =
  send_payload peer (NotFoundPayload p)
;;

let handle_payload peer payload =
  let discard () =
    debug_may peer (fun () ->
      Printf.printf "Received %s payload, discarding\n" (Bitcoin_protocol_pp.pp_string_of_command (command_of_message_payload payload)))
  in
  match payload with
  | VersionPayload p -> discard ()
  | VerAckPayload -> discard ()
  | AddrPayload p ->
    debug_may peer (fun () -> Printf.printf "[INFO] received %d new addresses from peer\n" (List.length p.addresses));
    discard ()
  | GetAddrPayload -> send_payload peer (own_addr_payload peer)
  | InvPayload p -> handle_inv_payload peer p
  | GetDataPayload p -> handle_getdata_payload peer p (* we store neither full blocks nor transactions currently, so we don't have any means to answer getdata requests *)
  | NotFoundPayload p -> discard ()
  | GetBlocksPayload p -> discard () (* we don't store full, so we don't have the ability to answer this request *)
  | GetHeadersPayload p -> discard () (* ditto *)
  | TxPayload p ->
    debug_may peer (fun () -> Bitcoin_protocol_pp.print_message_payload payload);
    discard ()
  | BlockPayload p -> handle_block peer p
  | HeadersPayload p -> discard ()
  | MemPoolPayload -> discard () (* we don't keep a memory pool yet *)
  | PingPayload p -> answer_ping peer p
  | PongPayload p -> discard ()
  | RejectPayload p -> discard ()
  | AlertPayload p -> discard ()
  | UnknownPayload p -> discard ()
;;

let rec message_loop peer =
  (
    match receive_message peer with
    | None ->
      debug_may peer (fun () -> print_endline "[INFO] invalid/no message received from peer");
    | Some m when (m.network) = peer.peer_network ->
      handle_payload peer m.payload
    | Some m ->
      debug_may peer (fun () -> Printf.printf "[INFO] invalid message (wrong network magic) received from peer: %s\n" (Bitcoin_protocol_pp.pp_string_of_magic m.network));
  );
  (* print_newline (); (\* force flush of stdout *\) *)
  (* print_string "Continue? "; flush stdout; ignore (input_line stdin); *)
  message_loop peer
;;

let handle_peer peer =
  debug_may peer (fun () -> print_string "Initializing peer connection...\t\t\t");
  let peer = initialize_connection peer in
  debug_may peer (fun () -> Printf.printf "protocol version %d\n" peer.peer_version.protocol_version);

  (* debug_may peer (fun () -> print_endline "Asking peer for new addresses..."); *)
  (* send_payload peer GetAddrPayload; *)

  debug_may peer (fun () -> print_endline "Initiating blockchain download...");
  initiate_blockchain_download peer;

  try
    message_loop peer
  with Bitcoin_protocol_parser.Connection_closed ->
    debug_may peer (fun () -> print_endline "[INFO] connection closed by peer")
;;
