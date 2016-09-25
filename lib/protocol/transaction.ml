open! Core.Std
open Bitcaml_utils.Std
open Bitcoin_crypto.Std
open Common

module Lock_time = struct
  type t =
    | AlwaysLocked
    | BlockLocked     of int32
    | TimestampLocked of Time.t
  [@@deriving bin_io, compare, sexp]


  let of_int32 = function
    | 0x0l                          -> AlwaysLocked
    | i when Int32.(i < 500000000l) -> BlockLocked i
    | i                             -> TimestampLocked (Int32.to_float i |> Time.of_epoch)
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
  module T = struct
    type t =
      { referenced_transaction_hash : Hash_string.t
      ; index                       : int32
      } [@@deriving bin_io, compare, fields, sexp]
  end
  include T

  include Comparable.Make_binable(T)

  let create = Fields.create

  let update ?referenced_transaction_hash ?index t =
    Fields.create
      ~referenced_transaction_hash:(Option.value ~default:t.referenced_transaction_hash referenced_transaction_hash)
      ~index:                      (Option.value ~default:t.index index)
  ;;

  let of_bitstring = function%bitstring
    | {| hash  : 32*8 : string
       ; index :  4*8 : littleendian
       ; rest  :   -1 : bitstring
      |} ->
      let t =
        Fields.create
          ~referenced_transaction_hash:(Hash_string.of_bytes hash)
          ~index
      in
      t, rest
    | {| _ |} -> failwith "invalid transaction outpoint"
  ;;

  let to_bitstring t =
    let hash = Hash_string.to_bytes t.referenced_transaction_hash in
    [%bitstring
      {| hash    : 32*8 : string
       ; t.index :  4*8 : littleendian
      |}]
  ;;
end

module Input = struct
  type t =
    { previous_output  : Outpoint.t
    ; signature_script : string
    ; sequence_number  : int32
    } [@@deriving bin_io, compare, fields, sexp]

  let create = Fields.create

  let update ?previous_output ?signature_script ?sequence_number t =
    Fields.create
    ~previous_output: (Option.value ~default:t.previous_output  previous_output)
    ~signature_script:(Option.value ~default:t.signature_script signature_script)
    ~sequence_number: (Option.value ~default:t.sequence_number  sequence_number)
  ;;

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
    } [@@deriving bin_io, compare, fields, sexp]

  let create = Fields.create

  let update ?value ?script t =
    Fields.create
      ~value: (Option.value ~default:t.value  value)
      ~script:(Option.value ~default:t.script script)
  ;;

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
  ; inputs    : Input.t  Int.Map.t
  ; outputs   : Output.t Int.Map.t
  ; lock_time : Lock_time.t
  } [@@deriving bin_io, compare, fields, sexp]

let create = Fields.create

let update ?version ?inputs ?outputs ?lock_time t =
  Fields.create
    ~version:  (Option.value ~default:t.version   version)
    ~inputs:   (Option.value ~default:t.inputs    inputs)
    ~outputs:  (Option.value ~default:t.outputs   outputs)
    ~lock_time:(Option.value ~default:t.lock_time lock_time)
;;

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
        ~inputs:(Index_map.of_list inputs)
        ~outputs:(Index_map.of_list outputs)
        ~lock_time:(Lock_time.of_int32 lock_time)
    in
    t, rest
  | {| _ |} -> failwith "invalid transaction"
;;

let to_bitstring t =
  let inputs  =
    Index_map.to_list t.inputs
    |> Varlist.to_bitstring ~bitstring_of_element:Input.to_bitstring
  in
  let outputs =
    Index_map.to_list t.outputs
    |> Varlist.to_bitstring ~bitstring_of_element:Output.to_bitstring
  in
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
  |> Bitstring.to_string
  |> Hash_string.hash256
;;

let total_output_value t =
  Map.fold t.outputs ~init:0L ~f:(fun ~key:_ ~data:output acc ->
    Int64.(+) acc (Output.value output))
;;

module Predicates = struct
  open Bitcoin_consensus.Std

  type nonrec t = t [@@deriving sexp_of]

  let empty_inputs  t = not (Map.is_empty t.inputs)
  let empty_outputs t = not (Map.is_empty t.outputs)

  let max_size t =
    let script_length_bits =
      to_bitstring t
      |> Bitstring.bitstring_length
      |> Int64.of_int
    in
    Int64.(script_length_bits / 8L >= Constants.max_block_size)
  ;;

  let output_values t =
    Map.exists t.outputs ~f:(fun output ->
      Int64.is_negative (Output.value output)
      || Int64.(>) (Output.value output) Constants.max_money)
  ;;

  let total_output_value t =
    let value = total_output_value t in
    Int64.is_negative value
    || Int64.(>) value Constants.max_money
  ;;

  let predicates =
    [ "bad-txns-vin-empty"                , empty_inputs
    ; "bad-txns-vout-empty"               , empty_outputs
    ; "bad-txns-oversize"                 , max_size
    ; "bad-txns-vout-notlegalmoney"       , output_values
    ; "bad-txns-txouttotal-notlegalmoney" , total_output_value
    ]
  ;;
end

include Validation.Make(Predicates)
