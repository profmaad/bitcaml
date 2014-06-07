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
