let bitcoin_protocol_version = 70002
let bitcoin_protocol_magic = Bitcoin_protocol.TestNet3

let user_agent = 
  let current_time = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "/bitcaml:%04d%02d%02d/" (current_time.Unix.tm_year + 1900) (current_time.Unix.tm_mon + 1) current_time.Unix.tm_mday
;;

let testnet_port = 18333

let peer_ip_address = "127.0.0.1"
let peer_port = testnet_port

let testnet3_genesis_block_hash = Utils.reverse_string "\x00\x00\x00\x00\x09\x33\xea\x01\xad\x0e\xe9\x84\x20\x97\x79\xba\xae\xc3\xce\xd9\x0f\xa3\xf4\x08\x71\x95\x26\xf8\xd7\x7f\x49\x43"
let testnet3_test_block_hash = Utils.reverse_string "\x00\x00\x00\x00\x70\x82\x7e\x85\xcd\xac\x70\x15\x31\x0b\xac\x8a\xe2\x3a\xa0\x22\x10\x1f\xeb\x27\x88\xca\xee\x9c\xd4\x19\x05\xfc"

let testnet3_genesis_block_header = {
  Bitcoin_protocol.block_version = 1;
  previous_block_hash = Utils.zero_hash;
  merkle_root = Utils.reverse_string "\x4a\x5e\x1e\x4b\xaa\xb8\x9f\x3a\x32\x51\x8a\x88\xc3\x1b\xc8\x7f\x61\x8f\x76\x67\x3e\x2c\xc7\x7a\xb2\x12\x7b\x7a\xfd\xed\xa3\x3b";
  block_timestamp = Utils.unix_tm_of_int32 1296688602l;
  block_difficulty_target = Bitcoin_protocol.difficulty_bits_of_int32 0x1d00ffffl;
  block_nonce = 414098458l;
}

let bitcaml_folder = "/home/profmaad/.bitcaml/"
let testnet3_folder = bitcaml_folder ^ "testnet3/"
let testnet3_blockchain_db = testnet3_folder ^ "blockchain.sqlite3"
let testnet3_blocks_folder = testnet3_folder ^ "blocks/"

let testnet3_initial_block_hashes = [
  Utils.reverse_string "\x00\x00\x00\x00\xb8\x73\xe7\x97\x84\x64\x7a\x6c\x82\x96\x2c\x70\xd2\x28\x55\x7d\x24\xa7\x47\xea\x4d\x1b\x8b\xbe\x87\x8e\x12\x06";
  Utils.reverse_string "\x00\x00\x00\x00\x6c\x02\xc8\xea\x6e\x4f\xf6\x96\x51\xf7\xfc\xde\x34\x8f\xb9\xd5\x57\xa0\x6e\x69\x57\xb6\x55\x52\x00\x2a\x78\x20";
  Utils.reverse_string "\x00\x00\x00\x00\x8b\x89\x6e\x27\x27\x58\xda\x52\x97\xbc\xd9\x8f\xdc\x6d\x97\xc9\xb7\x65\xec\xec\x40\x1e\x28\x6d\xc1\xfd\xbe\x10"
]
