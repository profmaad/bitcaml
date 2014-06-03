let bitcoin_protocol_version = 70002
let bitcoin_protocol_magic = Bitcoin.Protocol.TestNet3

let user_agent = 
  let current_time = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "/bitcaml:%04d%02d%02d/" (current_time.Unix.tm_year + 1900) (current_time.Unix.tm_mon + 1) current_time.Unix.tm_mday
;;

let testnet_port = 18333

let peer_ip_address = "127.0.0.1"
let peer_port = testnet_port

let testnet3_genesis_block_hash = Utils.reverse_string "\x00\x00\x00\x00\x09\x33\xea\x01\xad\x0e\xe9\x84\x20\x97\x79\xba\xae\xc3\xce\xd9\x0f\xa3\xf4\x08\x71\x95\x26\xf8\xd7\x7f\x49\x43"
let testnet3_test_block = Utils.reverse_string "\x00\x00\x00\x00\x70\x82\x7e\x85\xcd\xac\x70\x15\x31\x0b\xac\x8a\xe2\x3a\xa0\x22\x10\x1f\xeb\x27\x88\xca\xee\x9c\xd4\x19\x05\xfc"
