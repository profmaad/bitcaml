open! Core.Std
open Cryptokit;;

let ripemd160 data =
  hash_string (Hash.ripemd160 ()) data
;;
let sha1 data =
  hash_string (Hash.sha1 ()) data
;;
let sha256 data =
  hash_string (Hash.sha256 ()) data
;;

let hash160 data =
  ripemd160 (sha256 data)
;;
let hash256 data =
  sha256 (sha256 data)
;;
let double_sha256 = hash256;;
(* let double_sha256 data = *)
(*   Sha256.to_bin (Sha256.string (Sha256.to_bin (Sha256.string data))) *)
(* ;; *)

let message_checksum payload =
  let digest = hash256 payload in
  String.sub digest 0 4
;;

let rec merkle_tree_hash hash_f hashes =
  let rec create_row acc = function
    | [] -> acc
    | hash :: [] ->
      (hash_f (hash ^ hash)) :: acc
    | hash1 :: hash2 :: hashes ->
      create_row (hash_f (hash1 ^ hash2) :: acc) hashes
  in
  if (List.length hashes) = 1 then List.hd_exn hashes
  else
    let row = create_row [] hashes in
    let row = List.rev row in
    merkle_tree_hash hash_f row
;;
