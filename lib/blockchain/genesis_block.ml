open! Core.Std
open Bitcoin_crypto.Std
open Bitcoin_protocol.Std

module TestNet3 = struct
  let header =
    let merkle_root =
      Hash_string.of_string
        "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"
    in
    Block.Header.create
      ~version:1l
      ~previous_block_hash:Hash_string.zero
      ~merkle_root
      ~timestamp:(Time.of_epoch 1296688602.)
      ~difficulty_target:(Block.Difficulty.of_int32 0x1d00ffffl)
      ~nonce:414098458l
  ;;

  let hash =
    Hash_string.of_string
      "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"
  ;;
end

let header : Magic.t -> Block.Header.t = function
  | TestNet3 -> TestNet3.header
  | magic    -> failwithf !"genesis block for %{sexp:Magic.t} not available" magic ()
;;

let hash : Magic.t -> Hash_string.t = function
  | TestNet3 -> TestNet3.hash
  | magic    -> failwithf !"genesis block for %{sexp:Magic.t} not available" magic ()
;;
