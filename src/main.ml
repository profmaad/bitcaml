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
    relay = Some false;
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

let receive_message socket =
  let received_message = Bitcoin.Protocol.Parser.read_and_parse_message_from_fd socket in
  print_endline "Received message:";
  Option.may Bitcoin.Protocol.PP.print_message received_message;
  print_newline ();
  received_message
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
  let rec connection_fsm state =
    let new_state = state_entry socket state in
    if new_state != state then connection_fsm new_state
    else
      match state with
      | InitializedState -> ()
      | state ->
	let received_message = receive_message socket in
	match received_message with
	| None -> connection_fsm state
	| Some m -> connection_fsm (transition state m)
  in
  connection_fsm UninitializedState
;;

(* main *)
let () =
  Random.self_init ();
  let client_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let peer_addr = Unix.ADDR_INET(Unix.inet_addr_of_string Config.peer_ip_address, Config.peer_port) in
  Unix.connect client_socket peer_addr;
  handle_connection client_socket;
  Unix.sleep 1;
  Unix.close client_socket
;;
  

