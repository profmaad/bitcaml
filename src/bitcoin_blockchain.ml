open Bitcoin_protocol;;
open Bitcoin_rules;;

module DB = struct
  include Bitcoin_blockchain_db;;
end
module Blockstorage = struct
  include Bitcoin_blockchain_blockstorage;;
end

exception Rejected of string * rejection_reason;;
exception BlockIsOrphan;;
exception BlockIsDuplicate;;

type block_type =
| Sidechain
| Mainchain
| NewMainchain
;;

type blockchain = {
  db : DB.t;
  blockstorage : Blockstorage.t;
}
type t = blockchain;;

let init_default path =
  Utils.mkdir_maybe path 0o755;
  {
    db = DB.open_db (path ^ "blockchain.sqlite3");
    blockstorage = Blockstorage.init_default (path ^ "blocks/")
  }
;;

let tx_total_output_value tx =
  List.fold_left Int64.add 0L (List.map (fun txout -> txout.transaction_output_value) tx.transaction_outputs)
;;

(* tx rules 2-4 of https://en.bitcoin.it/wiki/Protocol_rules *)
let verify_basic_transaction_rules tx =
  if (List.length tx.transaction_inputs) = 0 then raise (Rejected ("bad-txns-vin-empty", RejectionInvalid));
  if (List.length tx.transaction_outputs) = 0 then raise (Rejected ("bad-txns-vout-empty", RejectionInvalid));
  if ((Bitstring.bitstring_length (Bitcoin_protocol_generator.bitstring_of_transaction tx)) / 8) >= max_block_size then raise (Rejected ("bad-txns-oversize", RejectionInvalid));
  if not (List.for_all transaction_output_in_legal_money_range tx.transaction_outputs) then raise (Rejected ("bad-txns-vout-notlegalmoney", RejectionInvalid));
  if not (legal_money_range (tx_total_output_value tx)) then raise (Rejected ("bad-txns-txouttotal-notlegalmoney", RejectionInvalid));
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
  match DB.nth_predecessor blockchain.db previous_hash (Int64.of_int (difficulty_change_interval - 1)) with
  | None -> raise (Rejected ("bad-diffbits", RejectionInvalid))
  | Some block ->
    let actual_timespan = Utils.time_difference header.block_timestamp block.DB.Block.block_header.block_timestamp in
    let old_difficulty = DB.difficulty_of_difficulty_bits block.DB.Block.block_header.block_difficulty_target in
    let new_difficulty = expected_new_difficulty old_difficulty actual_timespan in
    let claimed_new_difficulty = DB.difficulty_of_difficulty_bits header.block_difficulty_target in
    if new_difficulty <> claimed_new_difficulty then raise (Rejected ("bad-diffbits", RejectionInvalid))
;;

let validate_block_timestamp blockchain hash header =
  let extract_timestamp block = Utils.int64_of_unix_tm block.DB.Block.block_header.block_timestamp in
  let predecessors = DB.retrieve_n_predecessors blockchain.db header.previous_block_hash (timestamp_verification_predecessors - 1) in
  let predecessors = (Option.get (DB.Block.retrieve_by_hash blockchain.db header.previous_block_hash)) :: predecessors in
  if (List.length predecessors) = 0 then false
  else
    let sorted_timestamps = List.sort compare (List.map extract_timestamp predecessors) in
    let median_timestamp = List.nth sorted_timestamps ((List.length sorted_timestamps) / 2) in
    (Utils.int64_of_unix_tm header.block_timestamp) > median_timestamp
;;

let verify_transaction_script (tx, txin_index) signature_script output_script =
  let signature_script_asm = Bitcoin_script_parser.parse_script (Bitstring.bitstring_of_string signature_script) in
  let output_script_asm = Bitcoin_script_parser.parse_script (Bitstring.bitstring_of_string output_script) in
  let script_asm = signature_script_asm @ (Bitcoin_script.CodeSeparator :: output_script_asm) in
  (* Printf.printf "[DEBUG] verifying script for txin %d:\n" txin_index; *)
  (* Bitcoin_script_pp.print_script script_asm; *)

  let result = Bitcoin_script_interpreter.execute_script script_asm (tx, txin_index) in
  match result with
  | Bitcoin_script_interpreter.Result item ->
    (* Printf.printf "[DEBUG] script result: %s\n" (Bitcoin_script_pp.pp_string_of_data_item item); *)
    if not (Bitcoin_script.bool_of_data_item item) then raise (Rejected ("mandatory-script-verify-flag-failed", RejectionInvalid))
  | Bitcoin_script_interpreter.Invalid -> raise (Rejected ("mandatory-script-verify-flag-failed", RejectionInvalid))
;;

module TxOutMap = Map.Make(struct
  type t = string * int32;;
  let compare (hash1, index1) (hash2, index2) =
    let hash_compare = compare hash1 hash2 in
    if hash_compare = 0 then
      compare index1 index2
    else
      hash_compare
  ;;
end);;

let tx_has_duplicate_txins tx =
  let rec add_outpoints_if_not_exists txoutmap = function
    | [] -> true
    | outpoint :: outpoints ->
      if TxOutMap.mem outpoint txoutmap then false
      else add_outpoints_if_not_exists (TxOutMap.add outpoint true txoutmap) outpoints
  in

  let txins = List.map (fun txin -> (txin.previous_transaction_output.referenced_transaction_hash, txin.previous_transaction_output.transaction_output_index)) tx.transaction_inputs in
  not (add_outpoints_if_not_exists TxOutMap.empty txins)
;;
let block_has_duplicate_txins block =
  let rec add_outpoints_if_not_exists txoutmap = function
    | [] -> true
    | outpoint :: outpoints ->
      if TxOutMap.mem outpoint txoutmap then false
      else add_outpoints_if_not_exists (TxOutMap.add outpoint true txoutmap) outpoints
  in

  let txins = List.concat (List.map (fun tx -> List.map (fun txin -> (txin.previous_transaction_output.referenced_transaction_hash, txin.previous_transaction_output.transaction_output_index)) tx.transaction_inputs) block.block_transactions) in
  not (add_outpoints_if_not_exists TxOutMap.empty txins)
;;

let verify_mainchain_txin_return_value blockchain height tx processed_txout txin_index txin =
  let spent_output =
    try Some (TxOutMap.find (txin.previous_transaction_output.referenced_transaction_hash, txin.previous_transaction_output.transaction_output_index) processed_txout) with
    | Not_found -> DB.UTxO.retrieve_by_hash_and_index blockchain.db txin.previous_transaction_output.referenced_transaction_hash txin.previous_transaction_output.transaction_output_index
  in

  match spent_output with
  | None -> raise (Rejected ("bad-txns-inputs-missingorspent", RejectionInvalid))
  | Some utxo ->
    (* For each input, if the referenced output transaction is coinbase (i.e. only 1 input, with hash=0, n=-1), it must have at least COINBASE_MATURITY (100) confirmations; else reject. *)
    if utxo.DB.UTxO.is_coinbase then (
      if utxo.DB.UTxO.block_id < 0L then raise (Rejected ("bad-txns-premature-spend-of-coinbase", RejectionInvalid)) (* this indicates that the coinbase to spent is in the same block as this transation .. naughty naughty *)
      else
	match DB.Block.retrieve blockchain.db utxo.DB.UTxO.block_id with
	| None -> raise (Rejected ("bad-txns-premature-spend-of-coinbase", RejectionInvalid))
	| Some spent_block -> if (Int64.sub height spent_block.DB.Block.height) < coinbase_maturity then raise (Rejected ("bad-txns-premature-spend-of-coinbase", RejectionInvalid))
    );

    (* Using the referenced output transactions to get input values, check that each input value, as well as the sum, are in legal money range *)
    if not (legal_money_range utxo.DB.UTxO.value) then raise (Rejected ("bad-txns-inputvalues-outofrange", RejectionInvalid));
    
    (* Verify crypto signatures for each input; reject if any are bad *)
    verify_transaction_script (tx, txin_index) txin.signature_script utxo.DB.UTxO.script;

    utxo.DB.UTxO.value
;;
let verify_mainchain_tx_return_fee blockchain height processed_txout tx =
  let input_values = List.mapi (verify_mainchain_txin_return_value blockchain height tx processed_txout) tx.transaction_inputs in
  let input_value = List.fold_left Int64.add 0L input_values in

  let output_value = tx_total_output_value tx in

  (* Using the referenced output transactions to get input values, check that each input value, AS WELL AS THE SUM, are in legal money range *)
  if not (legal_money_range input_value) then raise (Rejected ("bad-txns-txintotal-notlegalmoney", RejectionInvalid));
  if not (legal_money_range output_value) then raise (Rejected ("bad-txns-txouttotal-notlegalmoney", RejectionInvalid));

  (* Reject if the sum of input values < sum of output values *)
  if(input_value < output_value) then raise (Rejected ("bad-txns-in-belowout", RejectionInvalid));

  let tx_fee = Int64.sub input_value output_value in
  if not (legal_money_range tx_fee) then raise (Rejected ("bad-txns-fee-outofrange", RejectionInvalid));

  tx_fee
;;

let verify_mainchain_block blockchain time block hash height =
  let utxo_tuple_of_txout hash index txout =
    ((hash, Int32.of_int index), {
      DB.UTxO.id = 0L;
      hash = hash;
      output_index = Int32.of_int index;
      block_id = Int64.minus_one;
      value = txout.transaction_output_value;
      script = txout.output_script;
      is_coinbase = false;
    })
  in
  let rec verify_mainchain_block_acc_return_fees processed_txout tx_fees = function
    | [] -> tx_fees
    | tx :: txs ->
      let tx_fee = verify_mainchain_tx_return_fee blockchain height processed_txout tx in
      let hash = Bitcoin_protocol_generator.transaction_hash tx in
      let new_txouts = List.mapi (utxo_tuple_of_txout hash) tx.transaction_outputs in
      let processed_txout = List.fold_left (fun acc (key, value) -> TxOutMap.add key value acc) processed_txout new_txouts in
      (* print_endline "//////////////"; *)
      verify_mainchain_block_acc_return_fees processed_txout (Int64.add tx_fees tx_fee) txs
  in

  if block_has_duplicate_txins block then raise (Rejected ("bad-txns-inputs-duplicate", RejectionInvalid));
  
  let processed_txout = TxOutMap.empty in
  let tx_fees = verify_mainchain_block_acc_return_fees processed_txout 0L (List.tl block.block_transactions) in
  let expected_coinbase_value = Int64.add (block_creation_fee_at_height height) tx_fees in
  let actual_coinbase_value = tx_total_output_value (List.hd block.block_transactions) in
  if actual_coinbase_value > expected_coinbase_value then raise (Rejected ("bad-cb-amount", RejectionInvalid));
;;

let verify_block blockchain time block hash =
  let previous_hash = block.block_header.previous_block_hash in

  (* Reject if duplicate of block we have in any of the three categories *)
  if (DB.Block.hash_exists blockchain.db hash) then raise (Rejected ("duplicate", RejectionDuplicate));
  if (DB.Orphan.hash_exists blockchain.db hash) then raise BlockIsOrphan;

  (* Transaction list must be non-empty *)
  if (List.length block.block_transactions) = 0 then raise (Rejected ("bad-blk-length", RejectionInvalid));

  (* Block hash must satisfy claimed nBits proof of work *)
  if not (verify_hash_against_difficulty_bits hash block.block_header.block_difficulty_target) then raise (Rejected ("high-hash", RejectionInvalid));

  (* Block timestamp must not be more than two hours in the future *)
  if (Utils.time_difference block.block_header.block_timestamp time) > max_block_into_future then raise (Rejected ("time-too-new", RejectionInvalid));

  (* serialised block size must be < MAX_BLOCK_SIZE *)
  if ((Bitstring.bitstring_length (Bitcoin_protocol_generator.bitstring_of_block block)) / 8) >= max_block_size then raise (Rejected ("bad-blk-length", RejectionInvalid));

  (* First transaction must be coinbase (i.e. only 1 input, with hash=0, n=-1), the rest must not be*)
  if not (transaction_is_coinbase (List.hd block.block_transactions)) then raise (Rejected ("bad-cb-missing", RejectionInvalid));
  if List.exists transaction_is_coinbase (List.tl block.block_transactions) then raise (Rejected ("bad-cb-multiple", RejectionInvalid));

  (* For each transaction, apply "tx" checks 2-4 *)
  List.iter verify_basic_transaction_rules block.block_transactions;

  (* For the coinbase (first) transaction, scriptSig length must be 2-100 *)
  let coinbase_script_length = String.length (List.hd (List.hd block.block_transactions).transaction_inputs).signature_script in
  if not (coinbase_script_length_range coinbase_script_length) then raise (Rejected ("bad-cb-length", RejectionInvalid));

  (* Reject if sum of transaction sig opcounts > MAX_BLOCK_SIGOPS *)
  if (List.fold_left ( + ) 0 (List.map Bitcoin_script.sigop_count (scripts_of_block block))) > max_block_sigops then raise (Rejected ("bad-blk-sigops", RejectionInvalid));

  (* Verify Merkle hash *)
  if not (block_merkle_root_matches block) then raise (Rejected ("bad-txnmrklroot", RejectionInvalid));

  (* 11. Check if prev block (matching prev hash) is in main branch or side branches. If not, add this to orphan blocks, then query peer we got this from for 1st missing orphan block in prev chain; done with block *)
  if not (DB.Block.hash_exists blockchain.db previous_hash) then raise BlockIsOrphan;

  let previous_block_height = (Option.get (DB.Block.retrieve_by_hash blockchain.db previous_hash)).DB.Block.height in
  
  (* 12. Check that nBits value matches the difficulty rules *)
  (* we know it must exist because we just checked it - at least as long as there is no concurrency *)
  let expected_blockchain_height = Int64.add 1L previous_block_height in 
  (* if (Int64.rem expected_blockchain_height (Int64.of_int difficulty_change_interval)) = 0L then verify_difficulty_change blockchain hash previous_hash block.block_header; *)
  (* TODO: REENABLE, doesn't behave well on testnet *)

  (* 13. Reject if timestamp is the median time of the last 11 blocks or before *)
  (* if not (validate_block_timestamp blockchain hash block.block_header) then raise (Rejected ("time-too-old", RejectionInvalid)); *)
  (* TODO: REENABLE, doesn't behave well on testnet though *)
  
  (* skip check 14 since we don't have checkpoints *)
  ()
;;

module BlockOutpointMap = Map.Make(String);;

let rollback_utxo_with_transaction blockchain block_id tx_index tx =
  let rec block_outpoint_map_of_outpoints map = function
    | [] -> map
    | (tx_hash, txout_index) :: outpoints ->
      match DB.block_id_hash_and_index_for_transaction_hash blockchain.db tx_hash with
      | None -> failwith (Printf.sprintf "transaction for hash %s not found during UTxO rollback" tx_hash)
      | Some (block_id, block_hash, tx_index) ->
	let current_binding = try BlockOutpointMap.find block_hash map with Not_found -> [] in
	let map = BlockOutpointMap.add block_hash ((block_id, tx_hash, tx_index, txout_index) :: current_binding) map in
	block_outpoint_map_of_outpoints map outpoints
  in
  let readd_outpoint_in_block block (block_id, tx_hash, tx_index, txout_index) =
    let txout = List.nth (List.nth block.block_transactions tx_index).transaction_outputs (Int32.to_int txout_index) in
    let db_utxo = {
      DB.UTxO.id = 0L;
      hash = tx_hash;
      output_index = txout_index;
      block_id = block_id;
      value = txout.transaction_output_value;
      script = txout.output_script;
      is_coinbase = (tx_index = 0);
    } in
    ignore (DB.UTxO.insert blockchain.db db_utxo)
  in
  let readd_outpoints_in_block block_hash outpoints =
    match Blockstorage.load_block blockchain.blockstorage block_hash with
    | None -> failwith (Printf.sprintf "block for hash %s failed to load from storage during UTxO rollback" block_hash)
    | Some block ->
      List.iter (readd_outpoint_in_block block) outpoints
  in

  let hash = Bitcoin_protocol_generator.transaction_hash tx in

  (* remove all UTxO entries created by this transaction *)
  DB.UTxO.delete_by_hash blockchain.db hash;

  (* iterate over inputs and readd the corresponding UTxO entry *)

  let spent_outpoints = List.map (fun txin -> (txin.previous_transaction_output.referenced_transaction_hash, txin.previous_transaction_output.transaction_output_index)) tx.transaction_inputs in
  let spent_outpoints_map = block_outpoint_map_of_outpoints BlockOutpointMap.empty spent_outpoints in
  BlockOutpointMap.iter readd_outpoints_in_block spent_outpoints_map
;;

let rollback_utxo_with_block blockchain block hash =
  Printf.printf "[DB] starting UTxO rollback for block %s\n%!" (Utils.hex_string_of_hash_string hash);
  match DB.Block.retrieve_by_hash blockchain.db hash with
  | None -> failwith "tried to rollback UTxO for non-existant block"
  | Some db_block ->
    List.iteri (DB.run_in_transaction blockchain.db (fun db -> rollback_utxo_with_transaction blockchain db_block.DB.Block.id)) (List.rev block.block_transactions);
    Printf.printf "[DB] finished UTxO rollback for block %s\n%!" (Utils.hex_string_of_hash_string hash);
;;

(* 15. Add block into the tree. *)
let classify_block blockchain block =

  let previous_block = Option.get (DB.Block.retrieve_by_hash blockchain.db block.block_header.previous_block_hash) in
  let current_mainchain_tip = DB.Block.retrieve_mainchain_tip blockchain.db in
  if Option.is_none current_mainchain_tip then failwith "No mainchain tip available, something is wrong with the blockchain..";
  let block_cld = (DB.log_difficulty_of_difficulty_bits block.block_header.block_difficulty_target) +. previous_block.DB.Block.cumulative_log_difficulty in

  (* There are three cases: *)
  match previous_block.DB.Block.is_main, (Option.get current_mainchain_tip).DB.Block.cumulative_log_difficulty, block_cld with
  (* 1. block further extends the main branch *)
  | true, _, _ -> Mainchain
  (* 3. block extends a side branch and makes it the new main branch. *)
  | false, mainchain_cld, our_cld when our_cld > mainchain_cld -> NewMainchain
  (* 2. block extends a side branch but does not add enough difficulty to make it become the new main branch *)
  | _, _, _ -> Sidechain
;;

let handle_orphan_block blockchain header hash log_difficulty =
  match DB.insert_block_as_orphan hash header.previous_block_hash log_difficulty header blockchain.db with
  | DB.InsertedAsOrphan record_id -> true
  | _ -> false
;;

let rec handle_block blockchain time block =
  let header = block.block_header in
  let hash = Bitcoin_protocol_generator.block_hash header in
  let log_difficulty = DB.log_difficulty_of_difficulty_bits header.block_difficulty_target in

  let insert_block () = 
    ( match DB.insert_block_into_blockchain hash header.previous_block_hash log_difficulty header blockchain.db with
    | DB.NotInsertedExisted -> raise BlockIsDuplicate
    | DB.InsertionFailed -> failwith "Block insertion failed at DB layer"
    | DB.InsertedIntoBlockchain record_id ->
      DB.register_transactions_for_block blockchain.db block record_id;
      Printf.printf "[DEBUG] inserted block %s into db as %Ld\n" (Utils.hex_string_of_hash_string hash) record_id
    | _ -> failwith "Block insertion failed at DB layer"
    );
    Blockstorage.store_block blockchain.blockstorage block;
    resolve_orphans blockchain time hash
  in

  ( try verify_block blockchain time block hash with
  | BlockIsOrphan ->
    Printf.printf "[DEBUG] block %s is an orphan\n" (Utils.hex_string_of_hash_string hash);
    ignore (handle_orphan_block blockchain header hash log_difficulty);
    Blockstorage.store_block blockchain.blockstorage block;
    raise BlockIsOrphan
  | Rejected (reason, RejectionDuplicate) -> raise BlockIsDuplicate
  | Rejected (reason, rejection_code) ->
    Printf.printf "[WARNING] block %s failed verification: %s\n" (Utils.hex_string_of_hash_string hash) reason;
    raise (Rejected (reason, rejection_code));
  );

  let height = (Option.get (DB.Block.retrieve_by_hash blockchain.db header.previous_block_hash)).DB.Block.height in
  let height = Int64.succ height in
  match classify_block blockchain block with
  (* For case 2, adding to a side branch, we don't do anything. *)
  | Sidechain -> insert_block ()
  | Mainchain ->
    (* For case 1, adding to main branch: *)
    verify_mainchain_block blockchain time block hash height;
    insert_block ();
    DB.update_utxo_with_block blockchain.db block hash
  | NewMainchain ->
    (* insert into db, then perform chain reorg *)
    insert_block ();
    match DB.retrieve_sidechain_with_leaf blockchain.db hash with
    | None ->
      Printf.printf "[FATAL] chain reorganisation required, but can't find mainchain ancestor of %s\n" (Utils.hex_string_of_hash_string hash);
      failwith "mainchain ancestor of sidechain block not found"
    | Some (sidechain, forkblock) ->
      Printf.printf "[INFO] switching mainchain starting from %s to %s\n" (Utils.hex_string_of_hash_string forkblock.DB.Block.hash) (Utils.hex_string_of_hash_string hash);
      DB.run_in_transaction blockchain.db (fun db -> reorganise_mainchain db blockchain time forkblock sidechain)

(* For each orphan block for which this block is its prev, run all these steps (including this one) recursively on that orphan *)
and resolve_orphans blockchain time inserted_hash =
  let resolve_orphan_block orphan_db_block =
    match Blockstorage.load_block blockchain.blockstorage orphan_db_block.DB.Orphan.hash with
    | None ->
      Printf.printf "[WARNING] failed to load orphan block %s from block storage\n" (Utils.hex_string_of_hash_string orphan_db_block.DB.Orphan.hash)
    | Some orphan_block ->
      handle_block blockchain time orphan_block;
      DB.Orphan.delete blockchain.db orphan_db_block.DB.Orphan.id
  in

  let resolved_orphans = DB.Orphan.retrieve_by_previous_block_hash blockchain.db inserted_hash in
  List.iter resolve_orphan_block resolved_orphans

(* we throw exceptions all over the place on purpose (Option.get...), because unless everything works in reorganisation, we break with an exception and rollback the DB transaction *)
and reorganise_mainchain db blockchain time forkblock sidechain =
  let rollback_utxo_with_hash hash =
    let block = Option.get (Blockstorage.load_block blockchain.blockstorage hash) in
    rollback_utxo_with_block blockchain block hash
  in

  let mainchain_tip = Option.get (DB.Block.retrieve_mainchain_tip db) in
  let former_mainchain = Option.get (DB.retrieve_between_hashes db mainchain_tip.DB.Block.hash forkblock.DB.Block.hash) in

  (* Redefine the main branch to only go up to this fork block *)
  DB.rollback_mainchain_to_height db forkblock.DB.Block.height;

  (* rollback utxo via former mainchain blocks after forkblock *)
  List.iter (fun db_block -> rollback_utxo_with_hash db_block.DB.Block.hash) (List.rev former_mainchain);

  (* then handle_block each block in sidechain in order *)
  (* if anything goes wrong, throw an exception and we rollback everything *)
  let sidechain_blocks = List.map (fun db_block -> Option.get (Blockstorage.load_block blockchain.blockstorage db_block.DB.Block.hash)) sidechain in
  List.iter (handle_block blockchain time) sidechain_blocks
;;
