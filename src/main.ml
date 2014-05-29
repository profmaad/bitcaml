(* #use "topfind";; *)
(* #require "unix";; *)
(* #require "sha.sha256";; *)
(* #require "bitstring";; *)

open Unix
open Printf
open Bitstring

type bitcoin_protocol_header = 
  { magic : int32;
    command : string;
    payload_length : int32;
    checksum : string;
  };;

let testnet_port = 18333
let peer_ip_address = "127.0.0.1"
let peer_port = testnet_port

let testnet_magic = "\x0b\x11\x09\x07"
let version_command = "version"
let verack_command = "verack"

let protocol_version = 70002

let user_agent = 
  let current_time = gmtime (time ()) in
  sprintf "\x12/bitcaml:%04d%02d%02d/" (current_time.tm_year + 1900) (current_time.tm_mon + 1) current_time.tm_mday

let print_hex_string s line_length =
  let hex_iterator index c =
    if (index > 0) && ((index mod line_length) = 0) then print_newline ();
    printf "%02x " (int_of_char c);
  in
  String.iteri hex_iterator s;
  print_newline ()
;;

let bitcoin_sha256 data =
  Sha256.to_bin (Sha256.string (Sha256.to_bin (Sha256.string data)))
;;
let bitcoin_message_checksum payload =
  let digest = bitcoin_sha256 payload in
  String.sub digest 0 4
;;

let reverse_string s =
  let rec reverse_string_acc s acc index length =
    if index >= length
    then acc
    else reverse_string_acc s ((String.make 1 s.[index]) ^ acc) (index+1) length
  in
  reverse_string_acc s "" 0 (String.length s)
;;

let bytestring_of_int i bytesize =
  let rec bytestring_of_int_ i acc byte_index =
    let shift_distance = 8*byte_index in
    let mask = 0xff lsl shift_distance in
    let masked_int = i land mask in
    let shifted_int = masked_int lsr shift_distance in
    let byte_char = Char.chr shifted_int in
    let new_acc = acc ^ (String.make 1 byte_char) in
    if byte_index = 0 then new_acc
    else bytestring_of_int_ i new_acc (byte_index-1)
  in
  bytestring_of_int_ i "" (bytesize-1)
;;
let le_bytestring_of_int i bytesize =
  reverse_string (bytestring_of_int i bytesize)
;;

let bitcoin_timestamp () =
  let timestamp = int_of_float (Unix.time ()) in
  let string_timestamp = le_bytestring_of_int timestamp 4 in
  print_hex_string (string_timestamp ^ (String.make 4 '\x00')) 8;
  string_timestamp ^ (String.make 4 '\x00')
let short_bitcoin_timestamp () =
  let long_timestamp = bitcoin_timestamp () in
  String.sub long_timestamp 0 4

let construct_bitcoin_message command payload =
  let magic = testnet_magic in
  let padded_command = command ^ String.make (12 - String.length command) '\x00' in
  let length = le_bytestring_of_int (String.length payload) 4 in
  let checksum = bitcoin_message_checksum payload in
  magic ^ padded_command ^ length ^ checksum ^ payload

let bitcoin_network_addr addr port =
  (* (short_bitcoin_timestamp ()) ^ *)
    "\x01\x00\x00\x00\x00\x00\x00\x00" ^
    "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" ^ addr ^
    (bytestring_of_int port 2)
;;

let test_version_message () =
  let payload = (le_bytestring_of_int protocol_version 4) ^ (*version*)
    "\x01\x00\x00\x00\x00\x00\x00\x00" ^ (*services*)
    (bitcoin_timestamp ()) ^ (*timestamp*)
    (bitcoin_network_addr "\xff\xff\x7f\x00\x00\x01" peer_port) ^ (*receiver netaddr struct*)
    (bitcoin_network_addr "\xff\xff\x7f\x00\x00\x01" peer_port) ^ (*sender netaddr struct*)
    "\x00\x00\xde\xad\xbe\xef\x00\x00" ^ (*"random" node nonce*)
    user_agent ^ (*empty user agent string*)
    "\x00\x00\x00\x00" ^ (*last received block*)
    "\x00" (*don't relay transactions to us*)
  in
  construct_bitcoin_message version_command payload
;;
let test_verack_message () =
  construct_bitcoin_message verack_command ""

let send_message socket message =
  let bytes_written = write socket message 0 (String.length message) in
  printf "Send message (%d bytes):\n" bytes_written;
  print_hex_string message 16;
;;
let receive_message socket =
  let received_message = String.make 4096 '\x00' in
  let bytes_read = read socket received_message 0 (String.length received_message) in
  printf "Received message (%d bytes):\n" bytes_read;
  print_hex_string (String.sub received_message 0 bytes_read) 16;
  received_message
;;

let read_string_from_fd fd bytes =
  let received_string = String.make bytes '\x00' in
  let bytes_read = read fd received_string 0 bytes in
  received_string
;;

let string_from_zeroterminated_string zts =
  let string_length =
    try
      String.index zts '\x00'
    with Not_found -> 12
  in
  String.sub zts 0 string_length
;;

let parse_bitcoin_header_from_string s =
  let header_bits = bitstring_of_string s in
  bitmatch header_bits with
  | { magic : 4*8 : littleendian;
      command : 12*8 : string;
      payload_length : 4*8 : littleendian;
      checksum : 4*8 : string
    } ->
    {
      magic = magic;
      command = string_from_zeroterminated_string command;
      payload_length = payload_length;
      checksum = checksum;
    }
  | { _ } -> eprintf "Received invalid bitcoin protocol header"; raise (Invalid_argument "Invalid bitcoin protocol header")
;;
let parse_bitcoin_header_from_fd fd =
  let bytestring = read_string_from_fd fd (4+12+4+4) in
  parse_bitcoin_header_from_string bytestring
;;

let string_of_timestamp timestamp =
  let time = localtime (Int64.to_float timestamp) in
  sprintf "%04d-%02d-%02d %02d:%02d:%02d" (time.tm_year + 1900) (time.tm_mon + 1) time.tm_mday time.tm_hour time.tm_min time.tm_sec
;;

let parse_bitcoin_version_message_from_string s =
  let message_bits = bitstring_of_string s in
  bitmatch message_bits with
  | { version : 4*8 : littleendian;
      services : 8*8 : littleendian;
      timestamp : 8*8 : littleendian;
      addr_recv : 26*8 : bitstring;
      addr_from : (if version >= (Int32.of_int 106) then 26*8 else 0) : bitstring;
      nonce : (if version >= (Int32.of_int 106) then 8*8 else 0) : littleendian;
      user_agent : ((String.length s) - (4+8+8+26+26+8+4 + (if version >= (Int32.of_int 70001) then 1 else 0)))*8 : string;
      start_height : 4*8 : littleendian;
      relay : (if version >= (Int32.of_int 70001) then 8 else 0) : littleendian
    } ->
    printf "Got version message: %ld\n%Ld\n%s\n%Ld\n%s\n%ld\n%d\n" version services (string_of_timestamp timestamp) nonce user_agent start_height (Int64.to_int relay)
;;
let read_payload_from_fd fd header =
  read_string_from_fd fd (Int32.to_int header.payload_length)
;;

(* main *)
let main =
  let client_socket = socket PF_INET SOCK_STREAM 0 in
  let peer_addr = ADDR_INET(inet_addr_of_string peer_ip_address, peer_port) in
  connect client_socket peer_addr;
  send_message client_socket (test_version_message ());
  let header = parse_bitcoin_header_from_fd client_socket in
  printf "Received header: %ld, %s, %ld\n" header.magic header.command header.payload_length;
  parse_bitcoin_version_message_from_string (read_payload_from_fd client_socket header);
  send_message client_socket (test_verack_message ());
  close client_socket
;;
  

