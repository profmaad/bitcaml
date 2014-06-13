open Bitcoin_protocol;;
open Bitcoin_rules;;
open Bitcoin_blockchain_db;;

exception Rejected of int * rejection_reason;;

(* tx rules 2-4 of https://en.bitcoin.it/wiki/Protocol_rules *)
let verify_basic_transaction_rules tx =
  if (List.length tx.transaction_inputs) = 0 then raise (Rejected (2, RejectionMalformed));
  if (List.length tx.transaction_outputs) = 0 then raise (Rejected (2, RejectionMalformed));
  if ((Bitstring.bitstring_length (Bitcoin_protocol_generator.bitstring_of_transaction tx)) / 8) >= max_block_size then raise (Rejected (3, RejectionMalformed));
  if not (List.for_all transaction_output_in_legal_money_range tx.transaction_outputs) then raise (Rejected (4, RejectionMalformed));
  true
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
