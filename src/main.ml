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
    relay = Some false;
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

let no_debug peer = { peer with Bitcoin.Peer.peer_debug = false };;
let debug peer = { peer with Bitcoin.Peer.peer_debug = true };;

(* main *)
let () =
  Random.self_init ();

  print_string "Sanity testing genesis block against its own hash...\t";
  let calculated_genesis_hash = Bitcoin.Blockchain.block_hash Config.testnet3_genesis_block_header in
  if calculated_genesis_hash = Config.testnet3_genesis_block_hash then
    print_endline "PASSED"
  else (
    Printf.printf"FAILED: %s != %s\n" calculated_genesis_hash Config.testnet3_genesis_block_hash;
    exit 1;
  );

  print_string "Testing difficulty calculation...\t";
  let difficulty_test_results = [
    Bitcoin.Blockchain.log_difficulty_of_difficulty_bits { Bitcoin.Protocol.bits_base = 0x00ffff; bits_exponent = 0x1d; };
    Bitcoin.Blockchain.log_difficulty_of_difficulty_bits { Bitcoin.Protocol.bits_base = 0x0404cb; bits_exponent = 0x1b; };
  ] in
  print_endline (String.concat ", " (List.map (Printf.sprintf "%f") difficulty_test_results));

  print_endline "Testing script parser and pretty printer...";
  let test_script = "\x76\xa9\x14\x2f\xef\x8e\xdc\xc4\x50\x19\xac\xba\x3b\xb1\x46\xb7\x6c\xbd\x2f\x84\x8b\xe5\xd6\x88\xac" in
  let parsed_script = Bitcoin.Script.Parser.parse_script (Bitstring.bitstring_of_string test_script) in
  Bitcoin.Script.PP.print_script parsed_script;
  print_endline "DONE";

  print_string "Sanity testing script generator against parser...\t"
  let test_script = "\x76\xa9\x14\x2f\xef\x8e\xdc\xc4\x50\x19\xac\xba\x3b\xb1\x46\xb7\x6c\xbd\x2f\x84\x8b\xe5\xd6\x88\xac" in
  let parsed_script = Bitcoin.Script.Parser.parse_script (Bitstring.bitstring_of_string test_script) in
  let generated_script = Bitstring.string_of_bitstring (Bitcoin.Script.Generator.bitstring_of_script parsed_script) in
  if test_script = generated_script then print_endline "PASSED"
  else
    Printf.printf "FAILED:\nExpected: %s\nActual  : %s\n" (Utils.hex_string_of_string test_script) (Utils.hex_string_of_string generated_script)
  ;

  Printf.printf "Opening and initializing blockchain db at %s...\t" Config.testnet3_blockchain_db;
  let blockchain_db = Bitcoin.Blockchain.open_db Config.testnet3_blockchain_db in
  print_endline "DONE";

  print_string "Establishing TCP connection to peer...\t\t";
  let peer_socket = connect_to_peer Config.peer_ip_address Config.peer_port in
  print_endline "DONE";

  let peer = {
    Bitcoin.Peer.peer_network = Bitcoin.Protocol.TestNet3;
    local_version = local_version ();
    peer_version = Bitcoin.Peer.default_version;
    peer_socket = peer_socket;
    peer_debug = false;
    blockchain = blockchain_db;
  } in

  Bitcoin.Peer.handle_peer peer;

  (* print_string "Retrieving TestNet3 genesis block...\t\t"; *)
  (* ( match Bitcoin.Peer.get_block peer Config.testnet3_genesis_block_hash with *)
  (* | None -> print_endline "FAILED" *)
  (* | Some block -> print_endline "PASSED"; Bitcoin.Protocol.PP.print_block block *)
  (* ); *)

  (* print_string "Retrieving a TestNet3 initial blocks...\t\t"; *)
  (* List.iter (fun hash -> *)
  (*   match Bitcoin.Peer.get_block peer hash with *)
  (*   | None -> print_endline "FAILED" *)
  (*   | Some block -> *)
  (*     print_endline "PASSED"; *)
  (*     Bitcoin.Protocol.PP.print_block block; *)
  (*     ignore (Bitcoin.Blockchain.insert_block block.Bitcoin.Protocol.block_header blockchain_db)) *)
  (*   (List.rev Config.testnet3_initial_block_hashes); *)
  
  print_string "Disconnecting from peer...\t\t\t";
  close_peer_connection peer_socket;
  print_endline "DONE"
;;
  

