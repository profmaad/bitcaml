let local_version () =
  let services_set = Bitcoin.Protocol.ServiceSet.add Bitcoin.Protocol.NetworkNodeService Bitcoin.Protocol.ServiceSet.empty in
  let localhost_address_string = (String.make 10 '\x00') ^ "\xff\xff" ^ "\127\000\000\001" in
  let receiver_address = {
    Bitcoin.Protocol.services = services_set;
    address = localhost_address_string;
    port = Config.peer_port;
  } in
  let sender_address = receiver_address in
  let random_nonce = Random.int64 Int64.max_int in
  {
    Bitcoin.Protocol.protocol_version = Config.bitcoin_protocol_version;
    node_services = services_set;
    timestamp = Unix.localtime (Unix.time ());
    receiver_address = receiver_address;
    sender_address = Some sender_address;
    random_nonce = Some random_nonce;
    user_agent = Some Config.user_agent;
    start_height = Some 0;
    relay = Some true;
  }
;;

let connect_to_peer ip_address port =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let peer_addr = Unix.ADDR_INET(Unix.inet_addr_of_string ip_address, port) in
  Unix.connect socket peer_addr;
  socket
;;
let close_peer_connection socket =
  Unix.sleep 1;
  Unix.close socket
;;  
    
(* main *)
let () =
  Random.self_init ();
  let peer_socket = connect_to_peer Config.peer_ip_address Config.peer_port in
  let peer = {
    Bitcoin.Peer.peer_network = Bitcoin.Protocol.TestNet3;
    local_version = local_version ();
    peer_version = Bitcoin.Peer.default_version;
    peer_socket = peer_socket;
    peer_debug = true;
  } in
  let peer = Bitcoin.Peer.handle_connection peer in
  Printf.printf "Peer is running protocol version %d\n" peer.Bitcoin.Peer.peer_version.Bitcoin.Protocol.protocol_version;
  (* exchange_addresses peer_protocol_version client_socket; *)
  (* download_block_chain peer_protocol_version [Config.testnet3_genesis_block_hash] client_socket; *)
  (* snoop_transactions peer_protocol_version client_socket; *)
  print_endline "Retrieving TestNet3 genesis block...";
  Bitcoin.Peer.get_block peer Config.testnet3_genesis_block_hash;
  print_endline "Retrieving a TestNet3 test block...";
  Bitcoin.Peer.get_block peer Config.testnet3_test_block;
  ( match Bitcoin.Peer.test_connection peer with
  | true -> print_endline "Connection to peer intact"
  | false -> print_endline "Connection to peer broken"
  );
  close_peer_connection peer_socket
;;
  

