open! Core.Std
open Bitcaml_utils.Std
open Bitcoin_protocol.Std
module Protocol_block = Block

module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Id)

module S = Sqlexpr

type db = S.db
type t = db

let init_db db =
  S.execute db
    [%sqlinit "CREATE TABLE IF NOT EXISTS blockchain(
    id INTEGER PRIMARY KEY,
    hash TEXT COLLATE BINARY NOT NULL,
    height INTEGER NOT NULL,
    cumulative_difficulty REAL NOT NULL,
    previous_block INTEGER NOT NULL,
    is_main BOOLEAN NOT NULL,
    version INTEGER NOT NULL,
    merkle_root TEXT COLLATE BINARY NOT NULL,
    timestamp INTEGER NOT NULL,
    difficulty_target INTEGER NOT NULL,
    nonce INTEGER NOT NULL
  );"];
  S.execute db
    [%sqlinit "CREATE TABLE IF NOT EXISTS orphans(
    id INTEGER PRIMARY KEY,
    hash TEXT COLLATE BINARY NOT NULL,
    previous_block_hash TEXT COLLATE BINARY NOT NULL,
    log_difficulty REAL NOT NULL,
    version INTEGER NOT NULL,
    merkle_root TEXT COLLATE BINARY NOT NULL,
    timestamp INTEGER NOT NULL,
    difficulty_target INTEGER NOT NULL,
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

module Record_id = struct
  type t = int64 [@@deriving compare, sexp_of]
end

module InsertionResult = struct
  type t =
    | InsertedIntoBlockchain of Record_id.t
    | InsertedAsOrphan       of Record_id.t
    | Existed
    | InsertionFailed
  [@@deriving compare, sexp_of]
end

module Orphan = struct
  type t =
    { id                  : Record_id.t
    ; hash                : string
    ; previous_block_hash : string
    ; log_difficulty      : float
    ; header              : Block.Header.t
    } [@@deriving compare, fields, sexp_of]

  let create = Fields.create

  let of_result
        ( id
        , hash
        , previous_block_hash
        , log_difficulty
        , version
        , merkle_root
        , timestamp
        , difficulty_target
        , nonce
        ) =
    let header =
      Block.Header.create
        ~version
        ~previous_block_hash
        ~merkle_root
        ~timestamp:(Time.of_epoch (Int64.to_float timestamp))
        ~difficulty_target:(Block.Difficulty.of_int32 difficulty_target)
        ~nonce
    in
    create
      ~id
      ~hash
      ~previous_block_hash
      ~log_difficulty
      ~header
  ;;

  let by_id db id =
    S.select_one_f db of_result
      [%sqlc "SELECT @L{id}, @s{hash}, @s{previous_block_hash}, @f{log_difficulty}, @l{version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_target}, @l{nonce} FROM orphans WHERE id = %L"]
      id
  ;;

  let by_hash db hash =
    S.select_one_f db of_result
      [%sqlc "SELECT @L{id}, @s{hash}, @s{previous_block_hash}, @f{log_difficulty}, @l{version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_target}, @l{nonce} FROM orphans WHERE hash = %s"]
      hash
  ;;

  let by_previous_block_hash db previous_block_hash =
    S.select_f db of_result
      [%sqlc "SELECT @L{id}, @s{hash}, @s{previous_block_hash}, @f{log_difficulty} @l{version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_target}, @l{nonce} FROM orphans WHERE previous_block_hash = %s"]
      previous_block_hash
  ;;

  let hash_exists db hash =
    S.select_one db [%sql "SELECT @b{count(1)} FROM orphans WHERE hash = %s LIMIT 1"] hash
  ;;

  let insert db t =
    let timestamp =
      Block.Header.timestamp t.header
      |> Time.to_epoch
      |> Int64.of_float
    in
    S.insert db
      [%sql "INSERT INTO orphans(hash, previous_block_hash, log_difficulty, version, merkle_root, timestamp, difficulty_target, nonce) VALUES(%s, %s, %f, %l, %s, %L, %l, %l)"]
      t.hash
      t.previous_block_hash
      t.log_difficulty
      (Block.Header.version t.header)
      (Block.Header.merkle_root t.header)
      timestamp
      (Block.Header.difficulty_target t.header |> Block.Difficulty.to_int32)
      (Block.Header.nonce t.header)
  ;;

  let delete db id =
    S.execute db [%sql "DELETE FROM orphans WHERE id = %L"] id
  ;;

end

module Block = struct
  type t =
    { id                    : Record_id.t
    ; hash                  : string
    ; height                : int64
    ; cumulative_difficulty : float
    ; previous_block_id     : Record_id.t
    ; is_main               : bool
    ; header                : Block.Header.t
    } [@@deriving compare, fields, sexp_of]

  let create = Fields.create

  let of_result
        ( id
        , hash
        , height
        , previous_block_id
        , cumulative_difficulty
        , is_main
        , version
        , merkle_root
        , timestamp
        , difficulty_target
        , nonce
        , previous_block_hash
        ) =
    let header =
      Block.Header.create
        ~version
        ~previous_block_hash
        ~merkle_root
        ~timestamp:(Time.of_epoch (Int64.to_float timestamp))
        ~difficulty_target:(Block.Difficulty.of_int32 difficulty_target)
        ~nonce
    in
    create
      ~id
      ~hash
      ~height
      ~cumulative_difficulty
      ~previous_block_id
      ~is_main
      ~header
  ;;

  let by_id db id =
    S.select_one_f db of_result
      [%sqlc "SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_difficulty}, @b{is_main}, @l{version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_target}, @l{nonce}, @s{IFNULL((SELECT hash FROM blockchain WHERE id = previous_block LIMIT 1), X'0000000000000000000000000000000000000000000000000000000000000000')} FROM blockchain WHERE id = %L"]
      id
  ;;

  let by_hash db hash =
    S.select_one_f db of_result
      [%sqlc "SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_difficulty}, @b{is_main}, @l{version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_target}, @l{nonce}, @s{IFNULL((SELECT hash FROM blockchain WHERE id = previous_block LIMIT 1), X'0000000000000000000000000000000000000000000000000000000000000000')} FROM blockchain WHERE hash = %s"]
      hash
  ;;

  let mainchain_tip db =
    S.select_one_f db of_result
      [%sqlc "SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_difficulty}, @b{is_main}, @l{version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_target}, @l{nonce}, @s{IFNULL((SELECT hash FROM blockchain WHERE id = previous_block LIMIT 1), X'0000000000000000000000000000000000000000000000000000000000000000')} FROM blockchain WHERE is_main = 1 ORDER BY cumulative_log_difficulty DESC, height DESC"]
  ;;

  let mainchain_at_height db height =
    S.select_one_f db of_result
      [%sqlc "SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block}, @f{cumulative_difficulty}, @b{is_main}, @l{version}, @s{merkle_root}, @L{timestamp}, @l{difficulty_target}, @l{nonce}, @s{IFNULL((SELECT hash FROM blockchain WHERE id = previous_block LIMIT 1), X'0000000000000000000000000000000000000000000000000000000000000000')} FROM blockchain WHERE height = %L AND is_main = 1 ORDER BY cumulative_log_difficulty DESC"]
      height
  ;;

  let hash_exists db hash =
    S.select_one db [%sql "SELECT @b{count(1)} FROM blockchain WHERE hash = %s LIMIT 1"] hash
  ;;

  let insert db t =
    let timestamp =
      Block.Header.timestamp t.header
      |> Time.to_epoch
      |> Int64.of_float
    in
    S.insert db
      [%sql "INSERT INTO blockchain(hash, height, previous_block, cumulative_difficulty, is_main, version, merkle_root, timestamp, difficulty_target, nonce) VALUES(%s, %L, %L, %f, %b, %l, %s, %L, %l, %l)"]
      t.hash
      t.height
      t.previous_block_id
      t.cumulative_difficulty
      t.is_main
      (Block.Header.version t.header)
      (Block.Header.merkle_root t.header)
      timestamp
      (Block.Header.difficulty_target t.header |> Block.Difficulty.to_int32)
      (Block.Header.nonce t.header)
  ;;
end

module UnspentTransactionOutput = struct
  type t =
    { id           : Record_id.t
    ; hash         : string
    ; output_index : int32
    ; block_id     : Record_id.t
    ; value        : int64
    ; script       : string
    ; is_coinbase  : bool
    } [@@deriving compare, fields, sexp_of]

  let create = Fields.create

  let of_result
        ( id
        , hash
        , output_index
        , block_id
        , value
        , script
        , is_coinbase
        ) =
    create
      ~id
      ~hash
      ~output_index
      ~block_id
      ~value
      ~script
      ~is_coinbase
    ;;

  let by_hash_and_index db hash output_index =
    S.select_one_f db of_result
      [%sqlc "SELECT @L{id}, @s{hash}, @l{output_index}, @L{block}, @L{value}, @s{script}, @b{is_coinbase} FROM unspent_transaction_outputs WHERE hash = %s AND output_index = %l"]
      hash
      output_index
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

  let insert db t =
    S.insert db
      [%sqlc "INSERT INTO unspent_transaction_outputs(hash, output_index, block, value, script, is_coinbase) VALUES(%s, %l, %L, %L, %s, %b)"]
      t.hash
      t.output_index
      t.block_id
      t.value
      t.script
      t.is_coinbase
  ;;
end
module UTxO = UnspentTransactionOutput

module MemoryPool = struct
  type t =
    { id           : Record_id.t
    ; hash         : string
    ; output_count : int32
    ; is_orphan    : bool
    ; data         : string
    } [@@deriving compare, fields, sexp_of]

  let create = Fields.create

  let of_result (id, hash, output_count, is_orphan, data) =
    Fields.create
      ~id
      ~hash
      ~output_count
      ~is_orphan
      ~data
  ;;

  let by_hash db hash  =
    S.select_one_f db of_result
      [%sqlc "SELECT @L{id}, @s{hash}, @l{output_count}, @b{is_orphan}, @S{data} FROM memory_pool WHERE hash = %s"]
      hash
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
  let block = Block.by_id db id in
  if Int64.equal n 0L then
    block
  else if Block.is_main block then
    Block.mainchain_at_height db Int64.(max 0L (Block.height block - n))
  else
    nth_predecessor_by_id db (Block.previous_block_id block) (Int64.pred n)
;;

let nth_predecessor db hash n =
  let block = Block.by_hash db hash in
  nth_predecessor_by_id db (Block.id block) n
;;

let retrieve_n_predecessors db hash n =
  let rec retrieve_n_predecessors_by_id_acc acc id = function
    | 0 -> List.rev acc
    | _ ->
       let block = Block.by_id db id in
       retrieve_n_predecessors_by_id_acc (block :: acc) (Block.previous_block_id block) (pred n)
  in
  let block = Block.by_hash db hash in
  retrieve_n_predecessors_by_id_acc [] (Block.id block) n
;;

let retrieve_sidechain_with_leaf db sidechain_hash =
  let rec retrieve_sidechain_acc db acc sidechain_id =
    let block = Block.by_id db sidechain_id in
    if Block.is_main block then
      (acc, block)
    else
      retrieve_sidechain_acc db (block :: acc) (Block.previous_block_id block)
  in
  let block = Block.by_hash db sidechain_hash in
  retrieve_sidechain_acc db [] (Block.id block)
;;

let retrieve_between_hashes db ~leaf_hash ~base_hash =
  let rec retrieve_between_hashes_acc db acc leaf_id base_hash =
    let block = Block.by_id db leaf_id in
    if String.equal (Block.hash block) base_hash then
      acc
    else
      retrieve_between_hashes_acc db (block :: acc) (Block.previous_block_id block) base_hash
  in
  if String.equal base_hash leaf_hash then
    []
  else
    let block = Block.by_hash db leaf_hash in
    retrieve_between_hashes_acc db [block] (Block.previous_block_id block) base_hash
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
  Protocol_block.transactions block
  |> List.iter ~f:(fun transaction ->
    Transaction.hash transaction
    |> MemoryPool.delete_by_hash db)
;;

let update_utxo_with_transaction db ~block_id ~transaction_index tx =
  let hash = Transaction.hash tx in
  let outpoint_of_txout txout_index txout =
    UTxO.create
      ~id:0L
      ~hash
      ~output_index:(Int32.of_int_exn txout_index)
      ~block_id
      ~value:(Transaction.Output.value txout)
      ~script:(Transaction.Output.script txout)
      ~is_coinbase:(Int.equal transaction_index 0)
  in
  Transaction.inputs tx
  |> Map.iter ~f:(fun input ->
    let outpoint = Transaction.Input.previous_output input in
    UTxO.delete_by_hash_and_index
      db
      (Transaction.Outpoint.referenced_transaction_hash outpoint)
      (Transaction.Outpoint.index outpoint));
  Transaction.outputs tx
  |> Map.iteri ~f:(fun ~key:index ~data:output ->
    outpoint_of_txout index output
    |> UTxO.insert db
    |> fun (_ : int64) -> ())
;;

(* since a transaction can spend an output that only appeard in the same block, we have to handle transactions in order *)
let update_utxo_with_block db block hash =
  printf "[DB] starting UTxO update for block %s\n%!" (Utils.hex_string_of_string (String.rev hash));
  let db_block = Block.by_hash db hash in
  Protocol_block.transactions block
  |> List.iteri ~f:(fun index tx ->
    S.transaction db (fun db ->
      update_utxo_with_transaction
        db
        ~block_id:(Block.id db_block)
        ~transaction_index:index
        tx));
  printf "[DB] finished UTxO update for block %s\n%!" (Utils.hex_string_of_string (String.rev hash))
;;

let register_transactions_for_block db block block_id =
  Protocol_block.transactions block
  |> List.iteri ~f:(fun index tx ->
    let hash = Transaction.hash tx in
    S.execute db [%sqlc "INSERT INTO transactions (hash, block, tx_index) VALUES (%s, %L, %d)"]
      hash
      block_id
      index)
;;

let insert_block_into_blockchain db ~hash ~previous_block_hash ~log_difficulty header : InsertionResult.t =
  match Block.hash_exists db hash with
  | true  -> Existed
  | false ->
    let previous_block = Block.by_hash db previous_block_hash in
    let record_id =
      Block.create
        ~id:0L
        ~hash
        ~height:(Int64.succ (Block.height previous_block))
        ~previous_block_id:(Block.id previous_block)
        ~cumulative_difficulty:(Block.cumulative_difficulty previous_block +. log_difficulty)
        ~is_main:(Block.is_main previous_block)
        ~header
      |> Block.insert db
    in
    InsertedIntoBlockchain record_id
;;

let insert_block_as_orphan db ~hash ~previous_block_hash ~log_difficulty header : InsertionResult.t =
  match Orphan.hash_exists db hash with
  | true  -> Existed
  | false ->
    let record_id =
      Orphan.create
        ~id:0L
        ~hash
        ~previous_block_hash
        ~log_difficulty
        ~header
      |> Orphan.insert db
    in
    InsertedAsOrphan record_id
;;

(* we need a special implementation for this, since no previous block exists for the genesis block *)
let insert_genesis_block db magic =
  let hash   = Genesis_block.hash   magic in
  let header = Genesis_block.header magic in
  let log_difficulty =
    Protocol_block.Header.difficulty_target header
    |> Protocol_block.Difficulty.to_float
  in
  match Block.hash_exists db hash with
  | true  -> ()
  | false ->
    Block.create
      ~id:0L
      ~hash
      ~height:0L
      ~previous_block_id:0L
      ~cumulative_difficulty:log_difficulty
      ~is_main:true
      ~header
    |> Block.insert db
    |> fun (_ : Record_id.t) -> ()
;;

let open_db ?(network=Magic.TestNet3) ~file () =
  let db = S.open_db file in
  init_db db;
  insert_genesis_block db network;
  db
;;
