open! Core.Std
open Bitcaml_utils.Std
open Bitcoin_crypto.Std
open Bitcoin_protocol.Std
open Bitcoin_protocol.Types

(* network rule constants *)
let max_block_size = 1000000;;
let coinbase_maturity = 100L;;
let max_money = Int64.( * ) 21000000L coin_size;;
let max_block_into_future = 2. *. 60. *. 60.;;
let max_block_sigops = max_block_size / 50;;
let difficulty_change_interval = 2016;;
let timestamp_verification_predecessors = 11;;
let initial_block_creation_fee_btc = 50L;;
let initial_block_creation_fee = Int64.( * ) initial_block_creation_fee_btc coin_size;;
let block_fee_reduction_interval = 210000L;;

let coin_size = 100000000L;;
let satoshis_per_bitoin = 100000000.0;;

let legal_money_range i = (i >= 0L) && (i <= max_money);;

let expected_new_difficulty old_difficulty actual_timespan =
  let two_weeks = 14. *. 24. *. 60. *. 60. in
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
let bigint_of_hash hash =
  let hash_le = Utils.reverse_string hash in
  let hash_le_hex = Utils.hex_encode hash_le in

  let result_7 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex ~pos:0 ~len:8))) in
  let result = Big_int.shift_left_big_int result_7 (7 * 32) in

  let result_6 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex ~pos:8 ~len:8))) in
  let result = Big_int.or_big_int result (Big_int.shift_left_big_int result_6 (6 * 32)) in

  let result_5 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex ~pos:16 ~len:8))) in
  let result = Big_int.or_big_int result (Big_int.shift_left_big_int result_5 (5 * 32)) in

  let result_4 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex ~pos:24 ~len:8))) in
  let result = Big_int.or_big_int result (Big_int.shift_left_big_int result_4 (4 * 32)) in

  let result_3 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex ~pos:32 ~len:8))) in
  let result = Big_int.or_big_int result (Big_int.shift_left_big_int result_3 (3 * 32)) in

  let result_2 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex ~pos:40 ~len:8))) in
  let result = Big_int.or_big_int result (Big_int.shift_left_big_int result_2 (2 * 32)) in

  let result_1 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex ~pos:48 ~len:8))) in
  let result = Big_int.or_big_int result (Big_int.shift_left_big_int result_1 (1 * 32)) in

  let result_0 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex ~pos:56 ~len:8))) in
  let result = Big_int.or_big_int result result_0 in

  result
;;
let bigint_of_difficulty_bits bits =
  let bigint_base = Big_int.big_int_of_int bits.bits_base in
  let exponent = 8 * (bits.bits_exponent - 3) in
  let bigint_exponent = Big_int.power_int_positive_int 2 exponent in
  Big_int.mult_big_int bigint_base bigint_exponent
;;

let verify_hash_against_difficulty_bits hash bits =
  let bigint_hash = bigint_of_hash hash in
  let bigint_target = bigint_of_difficulty_bits bits in
  Big_int.le_big_int bigint_hash bigint_target
;;

let hash_of_transaction tx =
  let tx_bitstring = Generator.bitstring_of_transaction tx in
  Hashing.hash256 (Bitstring.string_of_bitstring tx_bitstring)
;;
let merkle_root_of_block block =
  let transaction_hashes = List.map ~f:hash_of_transaction block.block_transactions in
  Hashing.merkle_tree_hash Hashing.hash256 transaction_hashes
;;

let transaction_input_is_coinbase txin =
  (txin.previous_transaction_output.referenced_transaction_hash = Utils.zero_hash) &&
    (txin.previous_transaction_output.transaction_output_index = Int32.minus_one)
;;

let transaction_is_coinbase tx =
  ((List.length tx.transaction_inputs) = 1) &&
    (transaction_input_is_coinbase (List.hd_exn tx.transaction_inputs))
;;

let coinbase_script_length_range i = (i >= 2) || (i <= 100);;

let transaction_output_in_legal_money_range txout = legal_money_range txout.transaction_output_value;;

let block_merkle_root_matches block =
  let calculated_merkle_root = merkle_root_of_block block in
  calculated_merkle_root = block.block_header.merkle_root
;;

let block_creation_fee_at_height height =
  let reduction_intervals = Int64.to_int_exn (Int64.(/) height block_fee_reduction_interval) in
  Int64.shift_right_logical initial_block_creation_fee reduction_intervals
;;

let transaction_is_final height time tx =
  if List.for_all ~f:(fun txin -> txin.transaction_sequence_number = 0xffffffffl) tx.transaction_inputs then
    true
  else
    match tx.transaction_lock_time with
    | AlwaysLockedTransaction -> true
    | BlockLockedTransaction lock_height -> (Int64.of_int32 lock_height) < height
    | TimestampLockedTransaction lock_time -> (Utils.time_difference lock_time time) < 0.
;;
