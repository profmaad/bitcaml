open! Core.Std
open Bignum.Std
open Bitcoin_crypto.Std
open Bitcoin_protocol.Std
open Bitcoin_consensus.Std

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

let verify_hash_against_difficulty_bits hash bits =
  let hash   = Hash_string.to_bigint      hash in
  let target = Block.Difficulty.to_bigint bits in
  Bigint.(<=) hash target
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

let block_merkle_root_matches block =
  let calculated_merkle_root = Block.calculate_merkle_root block in
  Block.header block
  |> Block.Header.merkle_root
  |> Hash_string.equal calculated_merkle_root
;;

let block_creation_fee_at_height height =
  let reduction_intervals = Int64.to_int_exn (Int64.(/) height Constants.block_reward_reduction_interval) in
  Int64.shift_right_logical Constants.initial_block_mining_reward reduction_intervals
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
