open! Core.Std
open Common

module Difficulty = struct
  type t =
    { base     : int32
    ; exponent : int32
    } [@@deriving compare, fields, sexp]

  let of_int32 i =
    Fields.create
      ~base:(Int32.bit_and i 0x00ffffffl)
      ~exponent:(Int32.shift_right_logical (Int32.bit_and i 0xff000000l) 24)
  ;;

  let to_int32 t =
    let exponent = Int32.shift_left (exponent t) 24 in
    Int32.bit_or exponent (base t)
  ;;
end

module Header = struct
  type t =
    { version             : int32
    ; previous_block_hash : string
    ; merkle_root         : string
    ; timestamp           : Time.t
    ; difficulty_target   : Difficulty.t
    ; nonce               : int32
    } [@@deriving compare, fields, sexp]

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
          ~previous_block_hash
          ~merkle_root
          ~timestamp:(Int32.to_float timestamp |> Time.of_epoch)
          ~difficulty_target:(Difficulty.of_int32 difficulty_target)
          ~nonce
      in
      t, rest
    | {| _ |} -> failwith "invalid block header"
  ;;

  let to_bitstring t =
    let timestamp = Time.to_epoch t.timestamp |> Int32.of_float in
    let difficulty_target = Difficulty.to_int32 t.difficulty_target in
    [%bitstring
      {| t.version             :  4*8 : littleendian
       ; t.previous_block_hash : 32*8 : string
       ; t.merkle_root         : 32*8 : string
       ; timestamp             :  4*8 : littleendian
       ; difficulty_target     :  4*8 : littleendian
       ; t.nonce               :  4*8 : littleendian
      |}]
  ;;
end

module Protocol_header = struct
  type t =
    { header            : Header.t
    ; transaction_count : int64
    } [@@deriving compare, fields, sexp]

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
  } [@@deriving compare, fields, sexp]

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

let hash t =
  to_bitstring t
  |> Bitstring.string_of_bitstring
  |> Bitcoin_crypto.Std.Hashing.hash256
;;
