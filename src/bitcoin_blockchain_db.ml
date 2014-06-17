open Bitcoin_protocol;;

module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Id);;
module S = Sqlexpr;;
type t = S.db;;

let difficulty_1_target = {
  bits_base = 0x00ffff;
  bits_exponent = 0x1d;
}
let float_log_difficulty_1_base = log (float_of_int difficulty_1_target.bits_base);;
let float_log_difficulty_scaland = log 256.0;;
let log_difficulty_of_difficulty_bits bits =
  let float_log_base = log (float_of_int bits.bits_base) in
  let float_exponent_difference = float_of_int (difficulty_1_target.bits_exponent - bits.bits_exponent) in
  float_log_difficulty_1_base -. float_log_base +. float_log_difficulty_scaland *. float_exponent_difference
;;
let difficulty_of_difficulty_bits bits = exp (log_difficulty_of_difficulty_bits bits);;

let init_db db = 
  S.execute db
    sqlinit"CREATE TABLE IF NOT EXISTS blockchain(
    id INTEGER PRIMARY KEY,
    hash TEXT COLLATE BINARY NOT NULL,
    height INTEGER NOT NULL,
    cumulative_log_difficulty REAL NOT NULL,
    previous_block INTEGER NOT NULL,
    is_main BOOLEAN NOT NULL,
    block_version INTEGER NOT NULL,
    merkle_root TEXT COLLATE BINARY NOT NULL,
    timestamp INTEGER NOT NULL,
    difficulty_bits INTEGER NOT NULL,
    nonce INTEGER NOT NULL
  );";
  S.execute db
    sqlinit"CREATE TABLE IF NOT EXISTS orphans(
    id INTEGER PRIMARY KEY,
    hash TEXT COLLATE BINARY NOT NULL,
    previous_block_hash TEXT COLLATE BINARY NOT NULL,
    log_difficulty REAL NOT NULL,
    block_version INTEGER NOT NULL,
    merkle_root TEXT COLLATE BINARY NOT NULL,
    timestamp INTEGER NOT NULL,
    difficulty_bits INTEGER NOT NULL,
    nonce INTEGER NOT NULL
  );";
  S.execute db
    sqlinit"CREATE INDEX IF NOT EXISTS hash_index ON blockchain (hash);";
  S.execute db
    sqlinit"CREATE INDEX IF NOT EXISTS mainchain_hash_index ON blockchain (hash, is_main);";
  S.execute db
    sqlinit"CREATE INDEX IF NOT EXISTS previous_block_index ON blockchain (previous_block);";
  S.execute db
    sqlinit"CREATE INDEX IF NOT EXISTS orphans_hash_index ON orphans (hash);";
  S.execute db
    sqlinit"CREATE INDEX IF NOT EXISTS orphans_previous_block_hash_index ON orphans (previous_block_hash);";

  S.execute db
    sqlinit"CREATE TABLE IF NOT EXISTS memory_pool(
    id INTEGER PRIMARY KEY,
    hash TEXT COLLATE BINARY NOT NULL,
    output_count INTEGER NOT NULL,
    is_orphan BOOLEAN NOT NULL,
    data BLOB NOT NULL
  );";
  S.execute db
    sqlinit"CREATE INDEX IF NOT EXISTS memory_pool_hash_index ON memory_pool (hash);";
  S.execute db
    sqlinit"CREATE INDEX IF NOT EXISTS memory_pool_orphan_index ON memory_pool (is_orphan);";

  (* S.execute db *)
  (*   sqlinit"CREATE TABLE IF NOT EXISTS transactions( *)
  (*   id INTEGER PRIMARY KEY, *)
  (*   hash TEXT COLLATE BINARY NOT NULL, *)
  (*   block INTEGER NOT NULL, *)
  (*   output_count INTEGER NOT NULL *)
  (*   is_coinbase BOOLEAN NOT NULL *)
  (* );"; *)
  S.execute db
    sqlinit"CREATE TABLE IF NOT EXISTS unspent_transaction_outputs(
    id INTEGER PRIMARY KEY,
    hash TEXT COLLATE BINARY NOT NULL,
    output_index INTEGER NOT NULL,
    block INTEGER NOT NULL,
    value INTEGER NOT NULL,
    script TEXT COLLATE BINARY NOT NULL,
    is_coinbase BOOLEAN NOT NULL
  );";
  (* S.execute db *)
  (*   sqlinit"CREATE INDEX IF NOT EXISTS transactions_hash_index ON transactions (hash);"; *)
  (* S.execute db *)
  (*   sqlinit"CREATE INDEX IF NOT EXISTS transactions_block_index ON transactions (block);"; *)
  S.execute db
    sqlinit"CREATE INDEX IF NOT EXISTS utxo_hash_index ON unspent_transaction_outputs (hash, output_index);";
;;

type insertion_result =
| InsertedIntoBlockchain of int64
| InsertedAsOrphan of int64
| InsertionFailed
| NotInsertedExisted
;;

let retrieve_block_by_id id db =
  S.select_one_maybe db
    sqlc"SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_log_difficulty}, @b{is_main}, @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce} FROM blockchain WHERE id = %L" id
;;
let retrieve_block hash db =
  S.select_one_maybe db
    sqlc"SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_log_difficulty}, @b{is_main}, @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce} FROM blockchain WHERE hash = %s" hash
;;
let retrieve_block_at_height height db =
  S.select_one_maybe db
    sqlc"SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_log_difficulty}, @b{is_main}, @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce} FROM blockchain WHERE height = %L ORDER BY cumulative_log_difficulty DESC" height
;;
let retrieve_mainchain_block_at_height height db =
  S.select_one_maybe db
    sqlc"SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_log_difficulty}, @b{is_main}, @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce} FROM blockchain WHERE height = %L AND is_main = 1 ORDER BY cumulative_log_difficulty DESC" height
;;

let block_id hash db =
  match retrieve_block hash db with
  | None -> None
  | Some (id, _, _, _, _, _, _, _, _, _, _) -> Some id
;;
let block_height hash db =
  match retrieve_block hash db with
  | None -> None
  | Some (_, _, height, _, _, _, _, _, _, _, _) -> Some height
;;
let block_height_by_id id db =
  match retrieve_block_by_id id db with
  | None -> None
  | Some (_, _, height, _, _, _, _, _, _, _, _) -> Some height
;;
let block_cumulative_log_difficulty hash db =
  match retrieve_block hash db with
  | None -> None
  | Some (_, _, _, _, cld, _, _, _, _, _, _) -> Some cld
;;

let block_exists hash db =
  match retrieve_block hash db with
  | None -> false
  | Some x -> true
;;

let retrieve_highest_cld_block db =
  S.select_one_maybe db
    sqlc"SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_log_difficulty}, @b{is_main}, @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce} FROM blockchain ORDER BY cumulative_log_difficulty DESC, height DESC"
;;

let mainchain_height db =
  match retrieve_highest_cld_block db with
  | None -> 0L
  | Some (_, _, height, _, _, _, _, _, _, _, _) -> height
;;
let mainchain_cumulative_log_difficulty db =
  match retrieve_highest_cld_block db with
  | None -> None
  | Some (_, _, _, _, cld, _, _, _, _, _, _) -> Some cld
;;

let rec nth_predecessor_by_id id n db =
  match retrieve_block_by_id id db with
  | None -> None
  | (Some (id, _, height, previous_block_id, _, is_main, _, _, _, _, _)) as block ->
    if n = 0L then block
    else
      if is_main then
	retrieve_mainchain_block_at_height (max 0L (Int64.sub height n)) db
      else
	nth_predecessor_by_id previous_block_id (Int64.sub n 1L) db
;;
let nth_predecessor hash n db =
  match retrieve_block hash db with
  | None -> None
  | Some (id, _, _, _, _, _, _, _, _, _, _) -> nth_predecessor_by_id id n db
;;

let retrieve_n_predecessors hash n db =
  let rec retrieve_n_predecessors_by_id_acc acc id n =
    if n = 0 then acc
    else
      match retrieve_block_by_id id db with
      | None -> acc
      | Some ((_, _, _, previous_block_id, _, _, _, _, _, _, _) as block) ->
	retrieve_n_predecessors_by_id_acc (block :: acc) previous_block_id (n - 1)
  in
  match retrieve_block hash db with
  | None -> []
  | Some (id, _, _, _, _, _, _, _, _, _, _) ->
    List.rev (retrieve_n_predecessors_by_id_acc [] id n)
;;

let retrieve_orphan hash db =
  S.select_one_maybe db
    sqlc"SELECT @L{id}, @s{hash}, @s{previous_block_hash}, @f{log_difficulty}, @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce} FROM orphans WHERE hash = %s" hash
;;

let orphan_exists hash db =
  S.select_one db sql"SELECT @b{count(1)} FROM orphans WHERE hash = %s LIMIT 1" hash
;;

let block_exists_anywhere hash db =
  (block_exists hash db) || (orphan_exists hash db)
;;

let delete_mempool_transaction db hash =
  S.execute db
    sql"DELETE FROM memory_pool WHERE hash = %s" hash
;;

let delete_block_transactions_from_mempool db block = 
  List.iter (delete_mempool_transaction db) (List.map Bitcoin_protocol_generator.transaction_hash block.block_transactions)
;;

let delete_utxo db hash index =
  S.execute db
    sqlc"DELETE FROM unspent_transaction_outputs WHERE hash = %s AND output_index = %l"
    hash
    index
;;
let insert_utxo db (hash, output_index, block, value, script, is_coinbase) =
  ignore (
    S.insert db
      sqlc"INSERT INTO unspent_transaction_outputs(hash, output_index, block, value, script, is_coinbase) VALUES(%s, %l, %L, %L, %s, %b)"
      hash
      output_index
      block
      value
      script
      is_coinbase
  )
;;
let retrieve_utxo db hash index =
  S.select_one_maybe db
    sqlc"SELECT @L{id}, @s{hash}, @l{output_index}, @L{block}, @L{value}, @s{script}, @b{is_coinbase} FROM unspent_transaction_outputs WHERE hash = %s AND output_index = %l"
    hash
    index
;;

let update_utxo_with_transaction db block_id tx_index tx =
  let hash = Bitcoin_protocol_generator.transaction_hash tx in

  let outpoint_of_txout txout_index txout = 
    (hash, Int32.of_int txout_index, block_id, txout.transaction_output_value, txout.output_script, tx_index = 0)
  in
  
  let spent_outpoints = List.map (fun txin -> (txin.previous_transaction_output.referenced_transaction_hash, txin.previous_transaction_output.transaction_output_index)) tx.transaction_inputs in
  let created_outpoints = List.mapi outpoint_of_txout tx.transaction_outputs in

  List.iter (fun (hash, index) -> delete_utxo db hash index) spent_outpoints;
  List.iter (insert_utxo db) created_outpoints
;;
(* since a transaction can spend an output that only appeard in the same block, we have to handle transactions in order *)
let update_utxo_with_block db block hash =
  Printf.printf "[DB] starting UTxO update for block %s\n%!" (Utils.hex_string_of_hash_string hash);
  match block_id hash db with
  | None -> failwith "tried to update utxo for non-existant block"
  | Some block_id ->
    List.iteri (S.transaction db (fun db -> update_utxo_with_transaction db block_id)) block.block_transactions;
    Printf.printf "[DB] finished UTxO update for block %s\n%!" (Utils.hex_string_of_hash_string hash);
;;

let insert_block_into_blockchain hash previous_block_hash log_difficulty header db =
  match block_exists hash db with
  | true -> NotInsertedExisted
  | false ->
    match retrieve_block previous_block_hash db with
    | None -> InsertionFailed
    | Some (previous_block_id, _, previous_block_height, _, previous_block_cld, previous_is_main, _, _, _, _, _) ->
      let record_id = S.insert db
	sql"INSERT INTO blockchain(hash, height, previous_block, cumulative_log_difficulty, is_main, block_version, merkle_root, timestamp, difficulty_bits, nonce) VALUES(%s, %L, %L, %f, %b, %d, %s, %L, %l, %l)" (* TODO: proper main chain handling *)
	hash
	(Int64.add previous_block_height 1L)
	previous_block_id
	(previous_block_cld +. log_difficulty)
	previous_is_main
	header.block_version
	header.merkle_root
	(Utils.int64_of_unix_tm header.block_timestamp)
	(int32_of_difficulty_bits header.block_difficulty_target)
	header.block_nonce
      in
      (* Printf.printf "[ DBG] Inserted block %s at height %Lu\n" (Utils.hex_string_of_hash_string hash) (Int64.add previous_block_height 1L); *)
      InsertedIntoBlockchain record_id
;;
let insert_block_as_orphan hash previous_block_hash log_difficulty header db =
  match orphan_exists hash db with
  | true -> NotInsertedExisted
  | false ->
    let record_id = S.insert db
      sql"INSERT INTO orphans(hash, previous_block_hash, log_difficulty, block_version, merkle_root, timestamp, difficulty_bits, nonce) VALUES(%s, %s, %f, %d, %s, %L, %l, %l)"
      hash
      previous_block_hash
      log_difficulty
      header.block_version
      header.merkle_root
      (Utils.int64_of_unix_tm header.block_timestamp)
      (int32_of_difficulty_bits header.block_difficulty_target)
      header.block_nonce
    in
    InsertedAsOrphan record_id
;;

let orphans_for_previous_block_hash db hash =
  S.select db
    sqlc"SELECT @L{id}, @s{hash}, @s{previous_block_hash}, @f{log_difficulty} @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce} FROM orphans WHERE previous_block_hash = %s" hash
;;
let delete_orphan_by_id db id =
  S.execute db
    sql"DELETE FROM orphans WHERE id = %L"
    id
;;

let insert_block header db =
  let hash = Bitcoin_protocol_generator.block_hash header in
  let log_difficulty = log_difficulty_of_difficulty_bits header.block_difficulty_target in
  if not (block_exists hash db) then
    if not (block_exists header.previous_block_hash db) then
      (* orphan block *)
      insert_block_as_orphan hash header.previous_block_hash log_difficulty header db
    else (
      (* we know the previous block, we can insert this block into the chain *)
      match insert_block_into_blockchain hash header.previous_block_hash log_difficulty header db with
      | InsertedIntoBlockchain i as result ->
	result
      | result -> result
    )
  else
    NotInsertedExisted
;;

(* we need a special implementation for this, since no previous block exists for the genesis block *)
let insert_genesis_block db =
  let hash = Config.testnet3_genesis_block_hash in
  let header = Config.testnet3_genesis_block_header in
  let log_difficulty = log_difficulty_of_difficulty_bits header.block_difficulty_target in
  if not (block_exists hash db) then
    Some (S.insert db
	    sql"INSERT INTO blockchain(hash, height, previous_block, cumulative_log_difficulty, is_main, block_version, merkle_root, timestamp, difficulty_bits, nonce) VALUES(%s, %L, %L, %f, 1, %d, %s, %L, %l, %l)"
	    hash
	    0L
	    0L
	    log_difficulty
	    header.block_version
	    header.merkle_root
	    (Utils.int64_of_unix_tm header.block_timestamp)
	    (int32_of_difficulty_bits header.block_difficulty_target)
	    header.block_nonce
    )
  else
    None
;;

let open_db path =
  let db = S.open_db path in
  init_db db;
  ignore (insert_genesis_block db);
  db
;;
