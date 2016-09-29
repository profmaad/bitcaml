open! Core.Std
open Bitcaml_utils.Std
open Bitcoin_protocol.Std
open Types
open Bitcoin_blockchain.Std
open Bitcoin_network.Std

let local_version () =
  let services_set = Service.Set.singleton Service.NetworkNodeService in
  let localhost_address_string = (String.make 10 '\x00') ^ "\xff\xff" ^ "\127\000\000\001" in
  let receiver_address = {
    services = services_set;
    address = localhost_address_string;
    port = Config.peer_port;
  } in
  let sender_address = receiver_address in
  let random_nonce = Random.int64 Int64.max_value in
  {
    protocol_version = Config.bitcoin_protocol_version;
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
  let socket = Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_STREAM ~protocol:0 in
  let peer_addr = Unix.ADDR_INET(Unix.Inet_addr.of_string ip_address, port) in
  Unix.connect socket ~addr:peer_addr;
  socket
;;
let close_peer_connection socket =
  Unix.sleep 1;
  Unix.close socket
;;

let no_debug peer = { peer with Peer.peer_debug = false };;
let debug peer = { peer with Peer.peer_debug = true };;

(* main *)
let () =
  Random.self_init ();

  print_string "Testing difficulty calculation...\t";
  let difficulty_test_results = [
    Db.log_difficulty_of_difficulty_bits { bits_base = 0x00ffff; bits_exponent = 0x1d; };
    Db.log_difficulty_of_difficulty_bits { bits_base = 0x0404cb; bits_exponent = 0x1b; };
  ] in
  print_endline (String.concat ~sep:", " (List.map ~f:(Printf.sprintf "%f") difficulty_test_results));

  print_endline "Testing script parser and pretty printer...";
  let test_script = "\x76\xa9\x14\x2f\xef\x8e\xdc\xc4\x50\x19\xac\xba\x3b\xb1\x46\xb7\x6c\xbd\x2f\x84\x8b\xe5\xd6\x88\xac" in
  let parsed_script = Bitcoin_script.Std.Parser.parse_script (Bitstring.bitstring_of_string test_script) in
  Bitcoin_script.Std.Pretty_print.print_script parsed_script;
  print_endline "DONE";

  print_string "Sanity testing script generator against parser...\t";
  let test_script = "\x76\xa9\x14\x2f\xef\x8e\xdc\xc4\x50\x19\xac\xba\x3b\xb1\x46\xb7\x6c\xbd\x2f\x84\x8b\xe5\xd6\x88\xac" in
  let parsed_script = Bitcoin_script.Std.Parser.parse_script (Bitstring.bitstring_of_string test_script) in
  let generated_script = Bitstring.string_of_bitstring (Bitcoin_script.Std.Generator.bitstring_of_script parsed_script) in
  if test_script = generated_script then print_endline "PASSED"
  else
    Printf.printf "FAILED:\nExpected: %s\nActual  : %s\n" (Utils.hex_string_of_string test_script) (Utils.hex_string_of_string generated_script)
  ;

  print_endline "Testing script engine...";
  let block_new, _ = Parser.parse_block (Bitstring.bitstring_of_file "/tmp/block_new.dat") in
  let block_new = Option.value_exn block_new in
  let tx_new = List.nth_exn block_new.block_transactions 1 in
  Pretty_print.print_transaction tx_new; Out_channel.newline stdout;
  let block_old, _ = Parser.parse_block (Bitstring.bitstring_of_file "/tmp/block_old.dat") in
  let block_old = Option.value_exn block_old in
  let tx_old = List.nth_exn block_old.block_transactions 4 in
  Pretty_print.print_transaction tx_old; Out_channel.newline stdout;

  (* let tx_new = { *)
  (*   transaction_data_format_version = 1; *)
  (*   transaction_inputs = [ *)
  (*     { *)
  (* 	previous_transaction_output = { *)
  (* 	  referenced_transaction_hash = Utils.hex_decode_rev "0437cd7f8525ceed2324359c2d0ba26006d92d856a9c20fa0241106ee5a597c9"; *)
  (* 	  transaction_output_index = 0l; *)
  (* 	}; *)
  (* 	signature_script = "\071" ^ Utils.hex_decode "304402204e45e16932b8af514961a1d3a1a25fdf3f4f7732e9d624c6c61548ab5fb8cd410220181522ec8eca07de4860a4acdd12909d831cc56cbbac4622082221a8768d1d0901"; *)
  (* 	transaction_sequence_number = 0xffffffffl; *)
  (*     } *)
  (*   ]; *)
  (*   transaction_outputs = [ *)
  (*     { *)
  (* 	transaction_output_value = 1000000000L; *)
  (* 	output_script = Utils.hex_decode "4104ae1a62fe09c5f51b13905f07f06b99a2f7159b2225f374cd378d71302fa28414e7aab37397f554a7df5f142c21c1b7303b8a0626f1baded5c72a704f7e6cd84cac"; *)
  (*     }; *)
  (*     { *)
  (* 	transaction_output_value = 4000000000L; *)
  (* 	output_script = Utils.hex_decode "410411db93e1dcdb8a016b49840f8c53bc1eb68a382e97b1482ecad7b148a6909a5cb2e0eaddfb84ccf9744464f82e160bfa9b8b64f9d4c03f999b8643f656b412a3ac"; *)
  (*     } *)
  (*   ]; *)
  (*   transaction_lock_time = AlwaysLockedTransaction; *)
  (* } in *)

  let pubkey_script = (List.hd_exn tx_new.transaction_inputs).signature_script in
  let pubkey_script_asm = Bitcoin_script.Std.Parser.parse_script (Bitstring.bitstring_of_string pubkey_script) in

  let output_script = (List.nth_exn tx_old.transaction_outputs 1).output_script in
  (* let output_script = Utils.hex_decode "410411db93e1dcdb8a016b49840f8c53bc1eb68a382e97b1482ecad7b148a6909a5cb2e0eaddfb84ccf9744464f82e160bfa9b8b64f9d4c03f999b8643f656b412a3ac" in *)
  let output_script_asm = Bitcoin_script.Std.Parser.parse_script (Bitstring.bitstring_of_string output_script) in

  let asm = pubkey_script_asm @ [Bitcoin_script.Std.Types.CodeSeparator] @ output_script_asm in
  Bitcoin_script.Pretty_print.print_script asm;

  print_endline "--------------------------";
  let script_result = Bitcoin_script.Std.Interpreter.execute_script asm (tx_new, 0) in
  print_endline "--------------------------";
  ( match script_result with
  | Bitcoin_script.Std.Interpreter.Result item ->
    Printf.printf "Script result: %s\n" (Bitcoin_script.Std.Pretty_print.pp_string_of_data_item item);
    if Bitcoin_script.Std.Types.bool_of_data_item item then print_endline "PASSED" else print_endline "FAILED"
  | Bitcoin_script.Std.Interpreter.Invalid -> print_endline "Script result: INVALID"; print_endline "FAILED"
  );

  print_endline "Testing merkle tree hashing...";
  let merkle_root_new = Rules.merkle_root_of_block block_new in
  ( if merkle_root_new = block_new.block_header.merkle_root then
      Printf.printf "PASSED: %s =\n        %s\n" (Utils.hex_string_of_string merkle_root_new) (Utils.hex_string_of_string block_new.block_header.merkle_root)
    else
      Printf.printf "FAILED: %s !=\n        %s\n" (Utils.hex_string_of_string merkle_root_new) (Utils.hex_string_of_string block_new.block_header.merkle_root)
  );
  let merkle_root_old = Rules.merkle_root_of_block block_old in
  ( if merkle_root_old = block_old.block_header.merkle_root then
      Printf.printf "PASSED: %s =\n        %s\n" (Utils.hex_string_of_string merkle_root_old) (Utils.hex_string_of_string block_old.block_header.merkle_root)
    else
      Printf.printf "FAILED: %s !=\n        %s\n" (Utils.hex_string_of_string merkle_root_old) (Utils.hex_string_of_string block_old.block_header.merkle_root)
  );

  print_string "Sanity testing weird script int encoding...\t";
  let test_value = "\xff\xff\xff\x82" in
  let decoded_test_value = Option.value_exn (Bitcoin_script.Std.Types.int64_of_data_item test_value) in
  let encoded_test_value = Bitcoin_script.Std.Types.data_item_of_int64 decoded_test_value in
  ( if (compare test_value encoded_test_value) <> 0 then
    Printf.printf "FAILED: %s -> %Ld -> %s\n" (Utils.hex_encode test_value) decoded_test_value (Utils.hex_encode encoded_test_value)
  else
    print_endline "PASSED"
  );

  Printf.printf "Opening and initializing blockchain at %s...\t" Config.testnet3_folder;
  let blockchain = Blockchain.init_default Config.testnet3_folder in
  print_endline "DONE";

  print_string "Establishing TCP connection to peer...\t\t";
  let peer_socket = connect_to_peer Config.peer_ip_address Config.peer_port in
  print_endline "DONE";

  let peer = {
    Peer.peer_network = TestNet3;
    local_version = local_version ();
    peer_version = Peer.default_version;
    peer_socket = peer_socket;
    peer_debug = true;
    blockchain = blockchain;
  } in

  Peer.handle_peer peer;

  (* print_string "Retrieving TestNet3 genesis block...\t\t"; *)
  (* ( match Bitcoin.Peer.get_block peer Bitcaml_config.testnet3_genesis_block_hash with *)
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
  (*   (List.rev Bitcaml_config.testnet3_initial_block_hashes); *)

  print_string "Disconnecting from peer...\t\t\t";
  close_peer_connection peer_socket;
  print_endline "DONE"
;;
