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

(* main *)
let () =
  Random.self_init ();
  let client_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let peer_addr = Unix.ADDR_INET(Unix.inet_addr_of_string Config.peer_ip_address, Config.peer_port) in
  Unix.connect client_socket peer_addr;
  send_message client_socket (test_version_message ());
  ignore (receive_message client_socket);
  ignore (receive_message client_socket);
  send_message client_socket (test_verack_message ());
  Unix.sleep 1;
  Unix.close client_socket
;;
  

