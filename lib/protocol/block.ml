open! Core.Std
open Bignum.Std
open Bitcaml_utils.Std
open Bitcoin_crypto.Std
open Common

module Difficulty = struct
  type t =
    { base     : int32
    ; exponent : int32
    } [@@deriving bin_io, compare, fields, sexp]

  let create = Fields.create

  let of_int32 i =
    Fields.create
      ~base:(Int32.bit_and i 0x00ffffffl)
      ~exponent:(Int32.shift_right_logical (Int32.bit_and i 0xff000000l) 24)
  ;;

  let to_int32 t =
    let exponent = Int32.shift_left (exponent t) 24 in
    Int32.bit_or exponent (base t)
  ;;

  let difficulty_1_target =
    create
      ~base:0x00ffffl
      ~exponent:0x1dl
  ;;

  let log_difficulty_1_base =
    Int32.to_float difficulty_1_target.base
    |> log
  ;;

  let log_difficulty_scalar = log 256.0

  let log_difficulty t =
    let log_base =
      Int32.to_float t.base
      |> log
    in
    let exponent_difference =
      Int32.(difficulty_1_target.exponent - t.exponent)
      |> Int32.to_float
    in
    log_difficulty_1_base
    -. log_base
    +. log_difficulty_scalar
    *. exponent_difference
  ;;

  let to_float t =
    log_difficulty t
    |> exp
  ;;

  let to_bigint t =
    let base = Bigint.of_int32 t.base in
    let exponent =
      Int32.(8l * (t.exponent - 3l))
      |> Bigint.of_int32
      |> Bigint.pow (Bigint.of_int 2)
    in
    Bigint.(base * exponent)
  ;;
end

module Header = struct
  type t =
    { version             : int32
    ; previous_block_hash : Hash_string.t
    ; merkle_root         : Hash_string.t
    ; timestamp           : Time.t
    ; difficulty_target   : Difficulty.t
    ; nonce               : int32
    } [@@deriving bin_io, compare, fields, sexp]

  let create = Fields.create

  let of_bitstring = function%bitstring
    | {| version             :  4*8 : littleendian
       ; previous_block_hash : 32*8 : string
       ; merkle_root         : 32*8 : string
       ; timestamp           :  4*8 : littleendian
       ; difficulty_target   :  4*8 : littleendian
       ; nonce               :  4*8 : littleendian
       ; rest                :   -1 : bitstring
      |} ->
      let t =
        Fields.create
          ~version
          ~previous_block_hash:(Hash_string.of_bytes previous_block_hash)
          ~merkle_root:(Hash_string.of_bytes merkle_root)
          ~timestamp:(Int32.to_float timestamp |> Time.of_epoch)
          ~difficulty_target:(Difficulty.of_int32 difficulty_target)
          ~nonce
      in
      t, rest
    | {| _ |} -> failwith "invalid block header"
  ;;

  let to_bitstring t =
    let previous_block_hash = Hash_string.to_bytes t.previous_block_hash in
    let merkle_root = Hash_string.to_bytes t.merkle_root in
    let timestamp = Time.to_epoch t.timestamp |> Int32.of_float in
    let difficulty_target = Difficulty.to_int32 t.difficulty_target in
    [%bitstring
      {| t.version             :  4*8 : littleendian
       ; previous_block_hash   : 32*8 : string
       ; merkle_root           : 32*8 : string
       ; timestamp             :  4*8 : littleendian
       ; difficulty_target     :  4*8 : littleendian
       ; t.nonce               :  4*8 : littleendian
      |}]
  ;;

  let hash t =
    to_bitstring t
    |> Bigstring.to_string
    |> Hash_string.hash256
  ;;
end

module Protocol_header = struct
  type t =
    { header            : Header.t
    ; transaction_count : int64
    } [@@deriving bin_io, compare, fields, sexp]

  let of_bitstring bits =
    let header, bits = Header.of_bitstring bits in
    let transaction_count, rest = Varint.of_bitstring bits in
    let t =
      Fields.create
        ~header
        ~transaction_count
    in
    t, rest
  ;;

  let to_bitstring t =
    let header = Header.to_bitstring t.header in
    let transaction_count = Varint.to_bitstring t.transaction_count in
    [%bitstring
      {| header            : -1 : bitstring
       ; transaction_count : -1 : bitstring
      |}]
  ;;
end

type t =
  { header       : Header.t
  ; transactions : Transaction.t list
  } [@@deriving bin_io, compare, fields, sexp]

let create = Fields.create

let of_bitstring bits =
  let header, bits = Header.of_bitstring bits in
  let transactions, rest =
    Varlist.of_bitstring
      ~element_of_bitstring:Transaction.of_bitstring
      bits
  in
  let t =
    Fields.create
      ~header
      ~transactions
  in
  t, rest
;;

let to_bitstring t =
  let header = Header.to_bitstring t.header in
  let transactions =
    Varlist.to_bitstring
      ~bitstring_of_element:Transaction.to_bitstring
      t.transactions
  in
  [%bitstring
    {| header       : -1 : bitstring
     ; transactions : -1 : bitstring
    |}]
;;

let hash t = Block.Header.hash t.header

let calculate_merkle_root t =
  List.map t.transactions ~f:Transaction.hash
  |> Hash_string.merkle_tree_hash
;;
