open! Core.Std
open Bitcaml_utils.Std
open Bitcoin_protocol.Std
open Types

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
    [%sqlinit "CREATE TABLE IF NOT EXISTS blockchain(
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
  );"];
  S.execute db
    [%sqlinit "CREATE TABLE IF NOT EXISTS orphans(
    id INTEGER PRIMARY KEY,
    hash TEXT COLLATE BINARY NOT NULL,
    previous_block_hash TEXT COLLATE BINARY NOT NULL,
    log_difficulty REAL NOT NULL,
    block_version INTEGER NOT NULL,
    merkle_root TEXT COLLATE BINARY NOT NULL,
    timestamp INTEGER NOT NULL,
    difficulty_bits INTEGER NOT NULL,
    nonce INTEGER NOT NULL
  );"];
  S.execute db
    [%sqlinit "CREATE INDEX IF NOT EXISTS hash_index ON blockchain (hash);"];
  S.execute db
    [%sqlinit "CREATE INDEX IF NOT EXISTS mainchain_hash_index ON blockchain (hash, is_main);"];
  S.execute db
    [%sqlinit "CREATE INDEX IF NOT EXISTS previous_block_index ON blockchain (previous_block);"];
  S.execute db
    [%sqlinit "CREATE INDEX IF NOT EXISTS orphans_hash_index ON orphans (hash);"];
  S.execute db
    [%sqlinit "CREATE INDEX IF NOT EXISTS orphans_previous_block_hash_index ON orphans (previous_block_hash);"];

  S.execute db
    [%sqlinit "CREATE TABLE IF NOT EXISTS memory_pool(
    id INTEGER PRIMARY KEY,
    hash TEXT COLLATE BINARY NOT NULL,
    output_count INTEGER NOT NULL,
    is_orphan BOOLEAN NOT NULL,
    data BLOB NOT NULL
  );"];
  S.execute db
    [%sqlinit "CREATE INDEX IF NOT EXISTS memory_pool_hash_index ON memory_pool (hash);"];
  S.execute db
    [%sqlinit "CREATE INDEX IF NOT EXISTS memory_pool_orphan_index ON memory_pool (is_orphan);"];

  S.execute db
    [%sqlinit "CREATE TABLE IF NOT EXISTS transactions(
    id INTEGER PRIMARY KEY,
    hash TEXT COLLATE BINARY NOT NULL,
    block INTEGER NOT NULL,
    tx_index INTEGER NOT NULL
  );"];
  S.execute db
    [%sqlinit "CREATE TABLE IF NOT EXISTS unspent_transaction_outputs(
    id INTEGER PRIMARY KEY,
    hash TEXT COLLATE BINARY NOT NULL,
    output_index INTEGER NOT NULL,
    block INTEGER NOT NULL,
    value INTEGER NOT NULL,
    script TEXT COLLATE BINARY NOT NULL,
    is_coinbase BOOLEAN NOT NULL
  );"];
  S.execute db
    [%sqlinit "CREATE INDEX IF NOT EXISTS transactions_hash_index ON transactions (hash);"];
  (* S.execute db *)
  (*   [%sqlinit "CREATE INDEX IF NOT EXISTS transactions_block_index ON transactions (block);"]; *)
  S.execute db
    [%sqlinit "CREATE INDEX IF NOT EXISTS utxo_hash_index ON unspent_transaction_outputs (hash, output_index);"];
;;

type insertion_result =
| InsertedIntoBlockchain of int64
| InsertedAsOrphan of int64
| InsertionFailed
| NotInsertedExisted
;;

let run_in_transaction db f =
  S.transaction db f
;;

module Block = struct
  type db_block = {
    id : int64;
    hash : string;
    height: int64;
    cumulative_log_difficulty : float;
    previous_block_id : int64;
    is_main : bool;
    block_header : block_header;
  };;
  type t = db_block;;

  let from_result (id, hash, height, previous_block, cld, is_main, block_version, merkle_root, timestamp, difficulty_bits, nonce, previous_block_hash) =
    {
      id = id;
      hash = hash;
      height = height;
      cumulative_log_difficulty = cld;
      previous_block_id = previous_block;
      is_main = is_main;
      block_header = {
	block_version = block_version;
	previous_block_hash = previous_block_hash;
	merkle_root = merkle_root;
	block_timestamp = Utils.unix_tm_of_int64 timestamp;
	block_difficulty_target = difficulty_bits_of_int32 difficulty_bits;
	block_nonce = nonce;
      };
    }
  ;;

  let retrieve db id =
    let result = S.select_one_maybe db
      [%sqlc "SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_log_difficulty}, @b{is_main}, @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce}, @s{IFNULL((SELECT hash FROM blockchain WHERE id = previous_block LIMIT 1), X'0000000000000000000000000000000000000000000000000000000000000000')} FROM blockchain WHERE id = %L"]
      id
    in
    Option.map ~f:from_result result
  ;;
  let retrieve_by_hash db hash =
    let result = S.select_one_maybe db
      [%sqlc "SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_log_difficulty}, @b{is_main}, @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce}, @s{IFNULL((SELECT hash FROM blockchain WHERE id = previous_block LIMIT 1), X'0000000000000000000000000000000000000000000000000000000000000000')} FROM blockchain WHERE hash = %s"]
      hash
    in
    Option.map ~f:from_result result
  ;;
  let retrieve_mainchain_tip db =
    let result = S.select_one_maybe db
      [%sqlc "SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_log_difficulty}, @b{is_main}, @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce}, @s{IFNULL((SELECT hash FROM blockchain WHERE id = previous_block LIMIT 1), X'0000000000000000000000000000000000000000000000000000000000000000')} FROM blockchain WHERE is_main = 1 ORDER BY cumulative_log_difficulty DESC, height DESC"]
    in
    Option.map ~f:from_result result
  ;;
  let retrieve_mainchain_block_at_height db height =
    let result = S.select_one_maybe db
    [%sqlc "SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_log_difficulty}, @b{is_main}, @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce}, @s{IFNULL((SELECT hash FROM blockchain WHERE id = previous_block LIMIT 1), X'0000000000000000000000000000000000000000000000000000000000000000')} FROM blockchain WHERE height = %L AND is_main = 1 ORDER BY cumulative_log_difficulty DESC"]
    height
    in
    Option.map ~f:from_result result
  ;;

  let hash_exists db hash =
    S.select_one db [%sql "SELECT @b{count(1)} FROM blockchain WHERE hash = %s LIMIT 1"] hash
  ;;

  let insert db db_block =
    S.insert db
      [%sql "INSERT INTO blockchain(hash, height, previous_block, cumulative_log_difficulty, is_main, block_version, merkle_root, timestamp, difficulty_bits, nonce) VALUES(%s, %L, %L, %f, %b, %d, %s, %L, %l, %l)"]
      db_block.hash
      db_block.height
      db_block.previous_block_id
      db_block.cumulative_log_difficulty
      db_block.is_main
      db_block.block_header.block_version
      db_block.block_header.merkle_root
      (Utils.int64_of_unix_tm db_block.block_header.block_timestamp)
      (int32_of_difficulty_bits db_block.block_header.block_difficulty_target)
      db_block.block_header.block_nonce
  ;;
end

module Orphan = struct
  type db_orphan = {
    id : int64;
    hash : string;
    previous_block_hash : string;
    log_difficulty : float;
    block_header : block_header;
  };;
  type t = db_orphan;;

  let from_result (id, hash, previous_block_hash, log_difficulty, block_version, merkle_root, timestamp, difficulty_bits, nonce) =
    {
      id = id;
      hash = hash;
      previous_block_hash = previous_block_hash;
      log_difficulty = log_difficulty;
      block_header = {
	block_version = block_version;
	previous_block_hash = previous_block_hash;
	merkle_root = merkle_root;
	block_timestamp = Utils.unix_tm_of_int64 timestamp;
	block_difficulty_target = difficulty_bits_of_int32 difficulty_bits;
	block_nonce = nonce;
      };
    }
  ;;

  let retrieve db id =
    let result = S.select_one_maybe db
      [%sqlc "SELECT @L{id}, @s{hash}, @s{previous_block_hash}, @f{log_difficulty}, @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce} FROM orphans WHERE id = %L"]
      id
    in
    Option.map ~f:from_result result
  ;;
  let retrieve_by_hash db hash =
    let result = S.select_one_maybe db
      [%sqlc "SELECT @L{id}, @s{hash}, @s{previous_block_hash}, @f{log_difficulty}, @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce} FROM orphans WHERE hash = %s"]
      hash
    in
    Option.map ~f:from_result result
  ;;
  let retrieve_by_previous_block_hash db previous_block_hash =
    let results = S.select db
      [%sqlc "SELECT @L{id}, @s{hash}, @s{previous_block_hash}, @f{log_difficulty} @d{block_version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_bits}, @l{nonce} FROM orphans WHERE previous_block_hash = %s"]
      previous_block_hash
    in
    List.map ~f:from_result results
  ;;

  let hash_exists db hash =
    S.select_one db [%sql "SELECT @b{count(1)} FROM orphans WHERE hash = %s LIMIT 1"] hash
  ;;

  let insert db db_orphan =
    S.insert db
      [%sql "INSERT INTO orphans(hash, previous_block_hash, log_difficulty, block_version, merkle_root, timestamp, difficulty_bits, nonce) VALUES(%s, %s, %f, %d, %s, %L, %l, %l)"]
      db_orphan.hash
      db_orphan.previous_block_hash
      db_orphan.log_difficulty
      db_orphan.block_header.block_version
      db_orphan.block_header.merkle_root
      (Utils.int64_of_unix_tm db_orphan.block_header.block_timestamp)
      (int32_of_difficulty_bits db_orphan.block_header.block_difficulty_target)
      db_orphan.block_header.block_nonce
  ;;

  let delete db id =
    S.execute db [%sql "DELETE FROM orphans WHERE id = %L"] id
  ;;

end

module UnspentTransactionOutput = struct
  type db_unspent_transaction_output = {
    id : int64;
    hash : string;
    output_index : int32;
    block_id : int64;
    value : int64;
    script : string;
    is_coinbase : bool;
  };;
  type t = db_unspent_transaction_output;;

  let from_result (id, hash, output_index, block, value, script, is_coinbase) =
    {
      id = id;
      hash = hash;
      output_index = output_index;
      block_id = block;
      value = value;
      script = script;
      is_coinbase = is_coinbase;
    }
    ;;

  let retrieve_by_hash_and_index db hash output_index =
    let result = S.select_one_maybe db
      [%sqlc "SELECT @L{id}, @s{hash}, @l{output_index}, @L{block}, @L{value}, @s{script}, @b{is_coinbase} FROM unspent_transaction_outputs WHERE hash = %s AND output_index = %l"]
      hash
      output_index
    in
    Option.map ~f:from_result result
  ;;

  let delete_by_hash db hash =
    S.execute db
      [%sqlc "DELETE FROM unspent_transaction_outputs WHERE hash = %s"]
      hash
  ;;
  let delete_by_hash_and_index db hash index =
    S.execute db
      [%sqlc "DELETE FROM unspent_transaction_outputs WHERE hash = %s AND output_index = %l"]
      hash
      index
  ;;

  let insert db utxo =
    S.insert db
      [%sqlc "INSERT INTO unspent_transaction_outputs(hash, output_index, block, value, script, is_coinbase) VALUES(%s, %l, %L, %L, %s, %b)"]
      utxo.hash
      utxo.output_index
      utxo.block_id
      utxo.value
      utxo.script
      utxo.is_coinbase
  ;;
end
module UTxO = UnspentTransactionOutput;;

module MemoryPool = struct
  type db_transaction = {
    id : int64;
    hash : string;
    output_count : int32;
    is_orphan : bool;
    data : string;
  };;
  type t = db_transaction;;

  let from_result (id, hash, output_count, is_orphan, data) =
    {
      id = id;
      hash = hash;
      output_count = output_count;
      is_orphan = is_orphan;
      data = data;
    }
  ;;

  let retrieve_by_hash db hash  =
    let result = S.select_one_maybe db
      [%sqlc "SELECT @L{id}, @s{hash}, @l{output_count}, @b{is_orphan}, @S{data} FROM memory_pool WHERE hash = %s"]
      hash
    in
    Option.map ~f:from_result result
  ;;

  let delete_by_hash db hash =
    S.execute db [%sql "DELETE FROM memory_pool WHERE hash = %s"] hash
  ;;

  let hash_exists db hash =
    S.select_one db [%sql "SELECT @b{count(1)} FROM memory_pool WHERE hash = %s LIMIT 1"] hash
  ;;
end

let mainchain_block_id_and_index_for_transaction_hash db tx_hash =
  S.select_one_maybe db
    [%sqlc "SELECT @L{transactions.block}, @d{transactions.tx_index} FROM transactions WHERE transaction.hash = %s AND blockchain.is_main = 1 INNER JOIN blockchain ON transactions.block = blockchain.id"]
    tx_hash
;;
let mainchain_block_hash_and_index_for_transaction_hash db tx_hash =
  S.select_one_maybe db
    [%sqlc "SELECT @s{blockchain.hash where}, @d{transactions.tx_index} FROM transactions WHERE hash = %s AND blockchain.is_main = 1 INNER JOIN blockchain ON transactions.block = blockchain.id"]
    tx_hash
;;
let mainchain_block_id_hash_and_index_for_transaction_hash db tx_hash =
  S.select_one_maybe db
    [%sqlc "SELECT @L{transactions.block}, @s{blockchain.hash}, @d{transactions.tx_index} FROM transactions WHERE hash = %s AND blockchain.is_main = 1 INNER JOIN blockchain ON transactions.block = blockchain.id"]
    tx_hash
;;
let mainchain_transaction_hash_exists db hash =
  S.select_one db [%sql "SELECT @b{count(1)} FROM transactions WHERE transactions.hash = %s AND blockchain.is_main = 1 INNER JOIN blockchain ON transactions.block = blockchain.id"] hash
;;

let rec nth_predecessor_by_id db id n =
  match Block.retrieve db id with
  | None -> None
  | Some block ->
    if n = 0L then Some block
    else
      if block.Block.is_main then
	Block.retrieve_mainchain_block_at_height db (max 0L (Int64.(-) block.Block.height n))
      else
	nth_predecessor_by_id db block.Block.previous_block_id (Int64.(-) n 1L)
;;
let nth_predecessor db hash n =
  match Block.retrieve_by_hash db hash with
  | None -> None
  | Some block -> nth_predecessor_by_id db block.Block.id n
;;

let retrieve_n_predecessors db hash n =
  let rec retrieve_n_predecessors_by_id_acc acc id n =
    if n = 0 then acc
    else
      match Block.retrieve db id with
      | None -> acc
      | Some block ->
	retrieve_n_predecessors_by_id_acc (block :: acc) block.Block.previous_block_id (n - 1)
  in
  match Block.retrieve_by_hash db hash with
  | None -> []
  | Some block ->
    List.rev (retrieve_n_predecessors_by_id_acc [] block.Block.id n)
;;

let retrieve_sidechain_with_leaf db sidechain_hash =
  let rec retrieve_sidechain_acc db acc sidechain_id =
    match Block.retrieve db sidechain_id with
    | None -> None
    | Some block ->
      if block.Block.is_main then Some (acc, block)
      else retrieve_sidechain_acc db (block :: acc) block.Block.previous_block_id
  in
  match Block.retrieve_by_hash db sidechain_hash with
  | None -> None
  | Some block -> retrieve_sidechain_acc db [] block.Block.id
;;
let retrieve_between_hashes db leaf_hash base_hash =
  let rec retrieve_between_hashes_acc db acc leaf_id base_hash =
    match Block.retrieve db leaf_id with
    | None -> None
    | Some block ->
      if block.Block.hash = base_hash then Some acc
      else retrieve_between_hashes_acc db (block :: acc) block.Block.previous_block_id base_hash
  in
  if base_hash = leaf_hash then Some []
  else (
    match Block.retrieve_by_hash db leaf_hash with
    | None -> None
    | Some block -> retrieve_between_hashes_acc db [block] block.Block.previous_block_id base_hash
  )
;;

let rollback_mainchain_to_height db height =
  S.execute db
    [%sqlc "UPDATE blockchain SET is_main = 0 WHERE is_main = 1 AND height > %L"]
    height
;;

let block_exists_anywhere db hash =
  (Block.hash_exists db hash) || (Orphan.hash_exists db hash)
;;

let delete_block_transactions_from_mempool db block =
  List.iter ~f:(MemoryPool.delete_by_hash db) (List.map ~f:Generator.transaction_hash block.block_transactions)
;;

let update_utxo_with_transaction db block_id tx_index tx =
  let hash = Generator.transaction_hash tx in

  let outpoint_of_txout txout_index txout =
    {
      UTxO.id = 0L;
      hash = hash;
      output_index = Int32.of_int_exn txout_index;
      block_id = block_id;
      value = txout.transaction_output_value;
      script = txout.output_script;
      is_coinbase = (tx_index = 0);
    }
  in

  let spent_outpoints = List.map ~f:(fun txin -> (txin.previous_transaction_output.referenced_transaction_hash, txin.previous_transaction_output.transaction_output_index)) tx.transaction_inputs in
  let created_outpoints = List.mapi ~f:outpoint_of_txout tx.transaction_outputs in

  List.iter ~f:(fun (hash, index) -> UTxO.delete_by_hash_and_index db hash index) spent_outpoints;
  List.iter ~f:(fun utxo -> ignore (UTxO.insert db utxo)) created_outpoints
;;
(* since a transaction can spend an output that only appeard in the same block, we have to handle transactions in order *)
let update_utxo_with_block db block hash =
  Printf.printf "[DB] starting UTxO update for block %s\n%!" (Utils.hex_string_of_hash_string hash);
  match Block.retrieve_by_hash db hash with
  | None -> failwith "tried to update UTxO for non-existant block"
  | Some db_block ->
    List.iteri ~f:(S.transaction db (fun db -> update_utxo_with_transaction db db_block.Block.id)) block.block_transactions;
    Printf.printf "[DB] finished UTxO update for block %s\n%!" (Utils.hex_string_of_hash_string hash);
;;

let register_transactions_for_block db block block_id =
  let register_tx tx_index tx =
    let hash = Generator.transaction_hash tx in
    S.execute db [%sqlc "INSERT INTO transactions (hash, block, tx_index) VALUES (%s, %L, %d)"]
      hash
      block_id
      tx_index
  in
  List.iteri ~f:register_tx block.block_transactions
;;

let insert_block_into_blockchain hash previous_block_hash log_difficulty header db =
  match Block.hash_exists db hash with
  | true -> NotInsertedExisted
  | false ->
    match Block.retrieve_by_hash db previous_block_hash with
    | None -> InsertionFailed
    | Some previous_block ->
      let record_id = Block.insert db {
	Block.id = 0L;
	hash = hash;
	height = (Int64.succ previous_block.Block.height);
	previous_block_id = previous_block.Block.id;
	cumulative_log_difficulty = (previous_block.Block.cumulative_log_difficulty +. log_difficulty);
	is_main = previous_block.Block.is_main;
	block_header = header;
      } in
      InsertedIntoBlockchain record_id
;;
let insert_block_as_orphan hash previous_block_hash log_difficulty header db =
  match Orphan.hash_exists db hash with
  | true -> NotInsertedExisted
  | false ->
    let record_id = Orphan.insert db {
      Orphan.id = 0L;
      hash = hash;
      previous_block_hash = previous_block_hash;
      log_difficulty = log_difficulty;
      block_header = header;
    } in
    InsertedAsOrphan record_id
;;

(* we need a special implementation for this, since no previous block exists for the genesis block *)
let insert_genesis_block db =
  let hash   = Config.testnet3_genesis_block_hash in
  let header = Config.testnet3_genesis_block_header in
  let log_difficulty = log_difficulty_of_difficulty_bits header.block_difficulty_target in
  if not (Block.hash_exists db hash) then
    let record_id = Block.insert db {
      Block.id = 0L;
      hash = hash;
      height = 0L;
      previous_block_id = 0L;
      cumulative_log_difficulty = log_difficulty;
      is_main = true;
      block_header = header;
    } in
    Some record_id
  else
    None
;;

let open_db path =
  let db = S.open_db path in
  init_db db;
  ignore (insert_genesis_block db);
  db
;;
