open! Core.Std
open Common

module Lock_time = struct
  type t =
    | AlwaysLocked
    | BlockLocked     of int32
    | TimestampLocked of Time.t
  [@@deriving compare, sexp]


  let of_int32 = function
    | 0x0l                    -> AlwaysLocked
    | i when (i < 500000000l) -> BlockLocked i
    | i                       -> TimestampLocked (Int32.to_float i |> Time.of_epoch)
  ;;

  let to_int32 = function
    | AlwaysLocked              -> 0x0l
    | BlockLocked i             -> i
    | TimestampLocked timestamp ->
      Time.to_epoch timestamp
      |> Int32.of_float
  ;;
end

module Outpoint = struct
  type t =
    { referenced_transaction_hash : string
    ; index                       : int32
    } [@@deriving compare, fields, sexp]

  let of_bitstring = function%bitstring
    | {| hash  : 32*8 : string
       ; index :  4*8 : littleendian
       ; rest  :   -1 : bitstring
      |} ->
      let t =
        Fields.create
          ~referenced_transaction_hash:hash
          ~index
      in
      t, rest
    | {| _ |} -> failwith "invalid transaction outpoint"
  ;;

  let to_bitstring t =
    [%bitstring
      {| t.referenced_transaction_hash : 32*8 : string
       ; t.index                       :  4*8 : littleendian
      |}]
  ;;
end

module Input = struct
  type t =
    { previous_output  : Outpoint.t
    ; signature_script : string
    ; sequence_number  : int32
    } [@@deriving compare, fields, sexp]

  let of_bitstring bits =
    let previous_output, bits = Outpoint.of_bitstring bits in
    let signature_script, bits = Varstring.of_bitstring ~name:"transaction input" bits in
    match%bitstring bits with
    | {| sequence_number : 4*8 : littleendian
       ; rest            :  -1 : bitstring
      |} ->
      let t =
        Fields.create
          ~previous_output
          ~signature_script
          ~sequence_number
      in
      t, rest
    | {| _ |} -> failwith "invalid transaction input"
  ;;

  let to_bitstring t =
    let previous_output = Outpoint.to_bitstring t.previous_output in
    let signature_script = Varstring.to_bitstring t.signature_script in
    [%bitstring
      {| previous_output   :  -1 : bitstring
       ; signature_script  :  -1 : bitstring
       ; t.sequence_number : 4*8 : littleendian
      |}]
  ;;
end

module Output = struct
  type t =
    { value  : int64
    ; script : string
    } [@@deriving compare, fields, sexp]

  let of_bitstring bits =
    let value, bits =
      match%bitstring bits with
      | {| value : 8*8 : littleendian
         ; bits  :  -1 : bitstring
        |} -> value, bits
      | {| _ |} -> failwith "invalid transaction output"
    in
    let script, rest = Varstring.of_bitstring ~name:"transaction output script" bits in
    let t =
      Fields.create
        ~value
        ~script
    in
    t, rest
  ;;

  let to_bitstring t =
    let script = Varstring.to_bitstring t.script in
    [%bitstring
      {| t.value : 8*8 : littleendian
       ; script  :  -1 : bitstring
      |}]
  ;;
end

type t =
  { version   : int32
  ; inputs    : Input.t list
  ; outputs   : Output.t list
  ; lock_time : Lock_time.t
  } [@@deriving compare, fields, sexp]

let of_bitstring bits =
  let version, bits =
    match%bitstring bits with
    | {| version : 4*8 : littleendian
       ; bits    :  -1 : bitstring
      |} -> version, bits
    | {| _ |} -> failwith "invalid transaction"
  in
  let inputs , bits = Varlist.of_bitstring ~element_of_bitstring:Input.of_bitstring  bits in
  let outputs, bits = Varlist.of_bitstring ~element_of_bitstring:Output.of_bitstring bits in
  match%bitstring bits with
  | {| lock_time : 4*8 : littleendian
     ; rest      :  -1 : bitstring
    |} ->
    let t =
      Fields.create
        ~version
        ~inputs
        ~outputs
        ~lock_time:(Lock_time.of_int32 lock_time)
    in
    t, rest
  | {| _ |} -> failwith "invalid transaction"
;;

let to_bitstring t =
  let inputs  = Varlist.to_bitstring ~bitstring_of_element:Input.to_bitstring  t.inputs  in
  let outputs = Varlist.to_bitstring ~bitstring_of_element:Output.to_bitstring t.outputs in
  let lock_time = Lock_time.to_int32 t.lock_time in
  [%bitstring
    {| t.version : 4*8 :littleendian
     ; inputs    :  -1 : bitstring
     ; outputs   :  -1 : bitstring
     ; lock_time : 4*8 : littleendian
    |}]
;;

let hash t =
  to_bitstring t
  |> Bitstring.string_of_bitstring
  |> Bitcoin_crypto.Std.Hashing.hash256
;;
