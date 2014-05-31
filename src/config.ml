let bitcoin_protocol_version = 70002
let bitcoin_protocol_magic = Bitcoin.Protocol.TestNet3

let user_agent = 
  let current_time = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "\x12/bitcaml:%04d%02d%02d/" (current_time.Unix.tm_year + 1900) (current_time.Unix.tm_mon + 1) current_time.Unix.tm_mday
;;

let testnet_port = 18333

let peer_ip_address = "127.0.0.1"
let peer_port = testnet_port
