open Bitcoin_protocol;;

exception Rejected of int * rejection_reason;;

(* network rule constants *)
let max_block_size = 1000000;;
let coinbase_maturity = 100;;
let max_money = Int64.mul 21000000L coin_size;;

let legal_money_range i = (i >= 0L) && (i <= max_money);;

(* we ingest the hex representation of the little-endian hash representation in blocks of 8 characters = 4 bytes *)
(* each block can then be represented as an int64 *)
(* since we only use bit operations, signed/unsigned interpretation isn't relevant *)
(* or so it should be, but for some inane reason, big_int bit operations expect positive arguments... *)
(* so we ingest in smaller chunks than int64 (int32), meaning everything is always positivie ... *d'oh *)
let bigint_of_hash hash =
  let hash_le = Utils.reverse_string hash in
  let hash_le_hex = Utils.hex_encode hash_le in

  let result_7 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex 0 8))) in
  let result = Big_int.shift_left_big_int result_7 (7 * 32) in

  let result_6 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex 8 8))) in
  let result = Big_int.or_big_int result (Big_int.shift_left_big_int result_6 (6 * 32)) in

  let result_5 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex 16 8))) in
  let result = Big_int.or_big_int result (Big_int.shift_left_big_int result_5 (5 * 32)) in

  let result_4 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex 24 8))) in
  let result = Big_int.or_big_int result (Big_int.shift_left_big_int result_4 (4 * 32)) in

  let result_3 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex 32 8))) in
  let result = Big_int.or_big_int result (Big_int.shift_left_big_int result_3 (3 * 32)) in

  let result_2 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex 40 8))) in
  let result = Big_int.or_big_int result (Big_int.shift_left_big_int result_2 (2 * 32)) in

  let result_1 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex 48 8))) in
  let result = Big_int.or_big_int result (Big_int.shift_left_big_int result_1 (1 * 32)) in

  let result_0 = Big_int.big_int_of_int64 (Int64.of_string ("0x" ^ (String.sub hash_le_hex 56 8))) in
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
  let tx_bitstring = Bitcoin_protocol_generator.bitstring_of_transaction tx in
  Bitcoin_crypto.hash256 (Bitstring.string_of_bitstring tx_bitstring)
;;
let merkle_root_of_block block =
  let transaction_hashes = List.map hash_of_transaction block.block_transactions in
  Bitcoin_crypto.merkle_tree_hash Bitcoin_crypto.hash256 transaction_hashes
;;

let transaction_input_is_coinbase txin =
  (txin.previous_transaction_output.referenced_transaction_hash = Utils.zero_hash) &&
    (txin.previous_transaction_output.transaction_output_index = Int32.minus_one)
;;

let transaction_is_coinbase tx =
  ((List.length tx.transaction_inputs) = 1) &&
    (transaction_input_is_coinbase (List.hd tx.transaction_inputs))
;;

let transaction_output_in_legal_money_range txout = legal_money_range txout.transaction_output_value;;

(* tx rules 2-4 of https://en.bitcoin.it/wiki/Protocol_rules *)
let verify_basic_transaction_rules tx =
  if (List.length tx.transaction_inputs) = 0 then raise (Rejected (2, RejectionMalformed));
  if (List.length tx.transaction_outputs) = 0 then raise (Rejected (2, RejectionMalformed));
  if ((Bitstring.bitstring_length (Bitcoin_protocol_generator.bitstring_of_transaction tx)) / 8) >= max_block_size then raise (Rejected (3, RejectionMalformed));
  if not (List.for_all transaction_output_in_legal_money_range tx.transaction_outputs) then raise (Rejected (4, RejectionMalformed));
  true
;;

let block_merkle_root_matches block =
  let calculated_merkle_root = merkle_root_of_block block in
  calculated_merkle_root = block.block_header.merkle_root
;;

let verify_block block =
  let hash = Bitcoin_protocol_generator.block_hash block.block_header in
  (* TODO: 2. Reject if duplicate of block we have in any of the three categories *)
  if (List.length block.block_transactions) = 0 then raise (Rejected (3, RejectionMalformed));
  if not (verify_hash_against_difficulty_bits hash block.block_header.block_difficulty_target) then raise (Rejected (4, RejectionInvalid));
  (* TODO: 5. Block timestamp must not be more than two hours in the future *)
  if not (transaction_is_coinbase (List.hd block.block_transactions)) then raise (Rejected (6, RejectionInvalid));
  if List.exists transaction_is_coinbase (List.tl block.block_transactions) then raise (Rejected (6, RejectionInvalid));
  if not (List.for_all verify_basic_transaction_rules block.block_transactions) then raise (Rejected (7, RejectionInvalid));
  let coinbase_script_length = String.length (List.hd (List.hd block.block_transactions).transaction_inputs).signature_script in
  if (coinbase_script_length < 2) || (coinbase_script_length > 100) then raise (Rejected (8, RejectionMalformed));
  (* TODO: 9. Reject if sum of transaction sig opcounts > MAX_BLOCK_SIGOPS *)
  if not (block_merkle_root_matches block) then raise (Rejected (10, RejectionInvalid));
  (* TODO: 11. Check if prev block (matching prev hash) is in main branch or side branches. If not, add this to orphan blocks, then query peer we got this from for 1st missing orphan block in prev chain; done with block *)
  (* TODO: 12. Check that nBits value matches the difficulty rules *)
  (* TODO: 13. Reject if timestamp is the median time of the last 11 blocks or before *)
  (* skip check 14 since we don't have checkpoints *)
  
  (* 15. Add block into the tree. There are three cases: 1. block further extends the main branch; 2. block extends a side branch but does not add enough difficulty to make it become the new main branch; 3. block extends a side branch and makes it the new main branch. *)
;;
