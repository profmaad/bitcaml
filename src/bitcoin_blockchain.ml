open Bitcoin_protocol;;
open Bitcoin_rules;;

module DB = struct
  include Bitcoin_blockchain_db;;
end
module Blockstorage = struct
  include Bitcoin_blockchain_blockstorage;;
end

exception Rejected of int * rejection_reason;;
exception BlockIsOrphan;;

type blockchain = {
  db : DB.t;
  blockstorage : Blockstorage.t;
}
type t = blockchain;;

(* tx rules 2-4 of https://en.bitcoin.it/wiki/Protocol_rules *)
let verify_basic_transaction_rules tx =
  if (List.length tx.transaction_inputs) = 0 then raise (Rejected (2, RejectionMalformed));
  if (List.length tx.transaction_outputs) = 0 then raise (Rejected (2, RejectionMalformed));
  if ((Bitstring.bitstring_length (Bitcoin_protocol_generator.bitstring_of_transaction tx)) / 8) >= max_block_size then raise (Rejected (3, RejectionMalformed));
  if not (List.for_all transaction_output_in_legal_money_range tx.transaction_outputs) then raise (Rejected (4, RejectionMalformed));
  true
;;

let scripts_of_transaction tx =
  let script_of_txin txin = Bitcoin_script_parser.parse_script (Bitstring.bitstring_of_string txin.signature_script) in
  let script_of_txout txout = Bitcoin_script_parser.parse_script (Bitstring.bitstring_of_string txout.output_script) in
  (List.map script_of_txin tx.transaction_inputs) @ (List.map script_of_txout tx.transaction_outputs)
;;
let scripts_of_block block =
  (* skip the first transaction: coinbase, doesn't have a valid script anyway *)
  List.concat (List.map scripts_of_transaction (List.tl block.block_transactions))
;;

let verify_difficulty_change blockchain hash previous_hash header =
  match DB.nth_predecessor previous_hash (Int64.of_int (difficulty_change_interval - 1)) blockchain.db with
  | None -> raise (Rejected (12, RejectionInvalid))
  | Some (_, _, _, _, _, _, _, _, timestamp, difficulty_bits, _) ->
    let actual_timespan = Utils.time_difference header.block_timestamp (Utils.unix_tm_of_int64 timestamp) in
    let old_difficulty = DB.difficulty_of_difficulty_bits (difficulty_bits_of_int32 difficulty_bits) in
    let new_difficulty = expected_new_difficulty old_difficulty actual_timespan in
    let claimed_new_difficulty = DB.difficulty_of_difficulty_bits header.block_difficulty_target in
    if new_difficulty != claimed_new_difficulty then raise (Rejected (12, RejectionInvalid))
;;

let validate_block_timestamp blockchain hash header =
  let extract_timestamp (_, _, _, _, _, _, _, _, timestamp, _, _) = timestamp in
  let predecessors = DB.retrieve_n_predecessors header.previous_block_hash (timestamp_verification_predecessors - 1) blockchain.db in
  let predecessors = (Option.get (DB.retrieve_block header.previous_block_hash blockchain.db)) :: predecessors in
  if (List.length predecessors) = 0 then false
  else
    let sorted_timestamps = List.sort compare (List.map extract_timestamp predecessors) in
    let median_timestamp = List.nth sorted_timestamps ((List.length sorted_timestamps) / 2) in
    (Utils.int64_of_unix_tm header.block_timestamp) > median_timestamp
;;

let verify_block blockchain time block =
  let hash = Bitcoin_protocol_generator.block_hash block.block_header in
  let previous_hash = block.block_header.previous_block_hash in

  (* Reject if duplicate of block we have in any of the three categories *)
  if (DB.block_exists_anywhere hash blockchain.db) then raise (Rejected (2, RejectionDuplicate));

  (* Transaction list must be non-empty *)
  if (List.length block.block_transactions) = 0 then raise (Rejected (3, RejectionInvalid));

  (* Block hash must satisfy claimed nBits proof of work *)
  if not (verify_hash_against_difficulty_bits hash block.block_header.block_difficulty_target) then raise (Rejected (4, RejectionInvalid));

  (* Block timestamp must not be more than two hours in the future *)
  if (Utils.time_difference block.block_header.block_timestamp time) > max_block_into_future then raise (Rejected (5, RejectionInvalid));

  (* First transaction must be coinbase (i.e. only 1 input, with hash=0, n=-1), the rest must not be*)
  if not (transaction_is_coinbase (List.hd block.block_transactions)) then raise (Rejected (6, RejectionInvalid));
  if List.exists transaction_is_coinbase (List.tl block.block_transactions) then raise (Rejected (6, RejectionInvalid));

  (* For each transaction, apply "tx" checks 2-4 *)
  if not (List.for_all verify_basic_transaction_rules block.block_transactions) then raise (Rejected (7, RejectionInvalid));

  (* For the coinbase (first) transaction, scriptSig length must be 2-100 *)
  let coinbase_script_length = String.length (List.hd (List.hd block.block_transactions).transaction_inputs).signature_script in
  if not (coinbase_script_length_range coinbase_script_length) then raise (Rejected (8, RejectionInvalid));

  (* Reject if sum of transaction sig opcounts > MAX_BLOCK_SIGOPS *)
  if (List.fold_left ( + ) 0 (List.map Bitcoin_script.sigop_count (scripts_of_block block))) > max_block_sigops then raise (Rejected (9, RejectionInvalid));

  (* Verify Merkle hash *)
  if not (block_merkle_root_matches block) then raise (Rejected (10, RejectionInvalid));

  (* TODO: Don't use an exception here, but instead redesign block insertion... *)
  (* 11. Check if prev block (matching prev hash) is in main branch or side branches. If not, add this to orphan blocks, then query peer we got this from for 1st missing orphan block in prev chain; done with block *)
  if not (DB.block_exists previous_hash blockchain.db) then raise BlockIsOrphan;

  (* 12. Check that nBits value matches the difficulty rules *)
  (* we know it must exist because we just checked it - at least as long as there is no concurrency *)
  let expected_blockchain_height = Int64.add 1L (Option.get (DB.block_height previous_hash blockchain.db)) in 
  if (Int64.rem expected_blockchain_height (Int64.of_int difficulty_change_interval)) = 0L then verify_difficulty_change blockchain hash previous_hash block.block_header;

  (* 13. Reject if timestamp is the median time of the last 11 blocks or before *)
  if not (validate_block_timestamp blockchain hash block.block_header) then raise (Rejected (13, RejectionInvalid));

  (* skip check 14 since we don't have checkpoints *)

  (* 15. Add block into the tree. There are three cases: 1. block further extends the main branch; 2. block extends a side branch but does not add enough difficulty to make it become the new main branch; 3. block extends a side branch and makes it the new main branch. *)
;;

let init_default path =
  Utils.mkdir_maybe path 0o755;
  {
    db = DB.open_db (path ^ "blockchain.sqlite3");
    blockstorage = Blockstorage.init_default (path ^ "blocks/")
  }
;;
