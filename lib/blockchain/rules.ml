open! Core.Std
open Bignum.Std
open Bitcoin_crypto.Std
open Bitcoin_protocol.Std

module Constants = struct
  let coin_size                           = 100000000L
  let satoshis_per_bitoin                 = 100000000.0
  let max_block_size                      = 1000000
  let coinbase_maturity                   = 100L
  let max_money                           = Int64.( * ) 21000000L coin_size
  let max_block_into_future               = 2. *. 60. *. 60.
  let max_block_sigops                    = max_block_size / 50
  let difficulty_change_interval          = 2016
  let timestamp_verification_predecessors = 11
  let initial_block_creation_fee_btc      = 50L
  let initial_block_creation_fee          = Int64.( * ) initial_block_creation_fee_btc coin_size
  let block_fee_reduction_interval        = 210000L
end

let is_legal_money i = (i >= 0L) && (i <= Constants.max_money)

let expected_new_difficulty =
  let two_weeks = 14. *. 24. *. 60. *. 60. in
  fun old_difficulty actual_timespan ->
    let timespan = match actual_timespan with
      | i when i < (two_weeks /. 4.) -> two_weeks /. 4.
      | i when i > (two_weeks *. 4.) -> two_weeks *. 4.
      | i -> i
    in
    old_difficulty *. (timespan /. two_weeks)
;;

(* we ingest the hex representation of the little-endian hash representation in blocks of 8 characters = 4 bytes *)
(* each block can then be represented as an int64 *)
(* since we only use bit operations, signed/unsigned interpretation isn't relevant *)
(* or so it should be, but for some inane reason, big_int bit operations expect positive arguments... *)
(* so we ingest in smaller chunks than int64 (int32), meaning everything is always positivie ... *d'oh *)
let bigint_of_hash hash : Bigint.t =
  let hash_le_hex =
    Hash_string.to_bytes hash
    |> String.rev
    |> Cryptokit.transform_string (Cryptokit.Hexa.encode ())
  in
  List.init 8 ~f:Fn.id
  |> List.fold ~init:Bigint.zero ~f:(fun result i ->
    String.sub hash_le_hex ~pos:(i * 8) ~len:8
    |> (fun s -> Int64.of_string ("0x" ^ s))
    |> Bigint.of_int64
    |> (fun value -> Bigint.shift_left value 32)
    |> Bigint.bit_or result)
;;

let bigint_of_difficulty_bits bits : Bigint.t =
  let base = Bigint.of_int32 (Block.Difficulty.base bits) in
  let exponent =
    Int32.(8l * (Block.Difficulty.exponent bits - 3l))
    |> Bigint.of_int32
    |> Bigint.pow (Bigint.of_int 2)
  in
  Bigint.(base * exponent)
;;

let verify_hash_against_difficulty_bits hash bits =
  let hash   = bigint_of_hash hash in
  let target = bigint_of_difficulty_bits bits in
  Bigint.(<=) hash target
;;

let merkle_root_of_block block =
  Block.transactions block
  |> List.map ~f:Transaction.hash
  |> Hash_string.merkle_tree_hash
;;

let transaction_is_coinbase tx =
  match Map.data (Transaction.inputs tx) with
  | [input] ->
    let outpoint = Transaction.Input.previous_output input in
    let is_zero_hash =
      Transaction.Outpoint.referenced_transaction_hash outpoint
      |> Hash_string.equal Hash_string.zero
    in
    is_zero_hash && Int32.(equal (Transaction.Outpoint.index outpoint) minus_one)
  | _ -> false
;;

let coinbase_script_length_range i = (i >= 2) || (i <= 100);;

let transaction_output_in_legal_money_range txout =
  is_legal_money (Transaction.Output.value txout)
;;

let block_merkle_root_matches block =
  let calculated_merkle_root = merkle_root_of_block block in
  Block.header block
  |> Block.Header.merkle_root
  |> Hash_string.equal calculated_merkle_root
;;

let block_creation_fee_at_height height =
  let reduction_intervals = Int64.to_int_exn (Int64.(/) height Constants.block_fee_reduction_interval) in
  Int64.shift_right_logical Constants.initial_block_creation_fee reduction_intervals
;;

let transaction_is_final height time tx =
  Transaction.inputs tx
  |> Map.for_all ~f:(fun input ->
    Int32.equal 0xffffffffl (Transaction.Input.sequence_number input))
  |> function
  | true  -> true
  | false ->
    match Transaction.lock_time tx with
    | AlwaysLocked              -> true
    | BlockLocked lock_height   -> Int64.(of_int32 lock_height < height)
    | TimestampLocked lock_time -> Time.(lock_time < time)
;;
