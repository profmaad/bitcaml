open! Core.Std
open Cryptokit

include String

let to_bytes = Fn.id
let of_bytes = Fn.id

let to_string t =
  rev t
  |> transform_string (Hexa.encode ())
;;
let of_string t =
  transform_string (Hexa.decode ()) t
  |> rev
;;

let to_string_hum t =
  rev t
  |> to_list
  |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
  |> String.concat ~sep:" "
;;

let zero = String.make 32 '\x00'

type hasher = string -> t

let ripemd160 data = hash_string (Hash.ripemd160 ()) data
let sha1      data = hash_string (Hash.sha1 ())      data
let sha256    data = hash_string (Hash.sha256 ())    data

let hash160 data = ripemd160 (sha256 data)
let hash256 data = sha256    (sha256 data)

let message_checksum payload =
  hash256 payload
  |> sub ~pos:0 ~len:4
;;

let rec merkle_tree_hash hashes =
  let rec create_row acc = function
    | [] -> acc
    | hash :: [] ->
      (hash256 (hash ^ hash)) :: acc
    | hash1 :: hash2 :: hashes ->
      create_row (hash256 (hash1 ^ hash2) :: acc) hashes
  in
  match hashes with
  | [hash] -> hash
  | hashes ->
    create_row [] hashes
    |> List.rev
    |> merkle_tree_hash
;;
