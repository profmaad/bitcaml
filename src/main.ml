let bitcoin_timestamp () =
  let timestamp = int_of_float (Unix.time ()) in
  let string_timestamp = Utils.le_bytestring_of_int timestamp 4 in
  string_timestamp ^ (String.make 4 '\x00')
;;
let short_bitcoin_timestamp () =
  let long_timestamp = bitcoin_timestamp () in
  String.sub long_timestamp 0 4
;;

let construct_bitcoin_message command payload =
  let magic = Utils.le_bytestring_of_int 0x0709110B 4 in
  let padded_command = command ^ String.make (12 - String.length command) '\x00' in
  let length = Utils.le_bytestring_of_int (String.length payload) 4 in
  let checksum = Bitcoin.Protocol.message_checksum payload in
  magic ^ padded_command ^ length ^ checksum ^ payload
;;

let bitcoin_network_addr addr port =
    "\x01\x00\x00\x00\x00\x00\x00\x00" ^
    "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" ^ addr ^
    (Utils.bytestring_of_int port 2)
;;

let test_version_message () =
  let payload = (Utils.le_bytestring_of_int Config.bitcoin_protocol_version 4) ^ (*version*)
    "\x01\x00\x00\x00\x00\x00\x00\x00" ^ (*services*)
    (bitcoin_timestamp ()) ^ (*timestamp*)
    (bitcoin_network_addr "\xff\xff\x7f\x00\x00\x01" Config.peer_port) ^ (*receiver netaddr struct*)
    (bitcoin_network_addr "\xff\xff\x7f\x00\x00\x01" Config.peer_port) ^ (*sender netaddr struct*)
    "\x00\x00\xde\xad\xbe\xef\x00\x00" ^ (*"random" node nonce*)
    Config.user_agent ^ (*empty user agent string*)
    "\x00\x00\x00\x00" ^ (*last received block*)
    "\x00" (*don't relay transactions to us*)
  in
  construct_bitcoin_message "version" payload
;;
let test_verack_message () =
  construct_bitcoin_message "verack" ""

let send_message socket message =
  let bytes_written = Unix.write socket message 0 (String.length message) in
  Printf.printf "Send message (%d bytes):\n" bytes_written;
  Utils.print_hex_string message 16;
  print_newline ();
;;
let receive_message socket =
  let received_message = String.make 4096 '\x00' in
  let bytes_read = Unix.read socket received_message 0 (String.length received_message) in
  Printf.printf "Received message (%d bytes):\n" bytes_read;
  Utils.print_hex_string (String.sub received_message 0 bytes_read) 16;
  print_newline ();
  received_message
;;

(* main *)
let () =
  let client_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let peer_addr = Unix.ADDR_INET(Unix.inet_addr_of_string Config.peer_ip_address, Config.peer_port) in
  Unix.connect client_socket peer_addr;
  send_message client_socket (test_version_message ());
  let received_message = Bitcoin.Protocol.Parser.read_and_parse_message_from_fd client_socket in
  Option.may Bitcoin.Protocol.PP.print_message received_message;
  send_message client_socket (test_verack_message ());
  Unix.close client_socket
;;
  

