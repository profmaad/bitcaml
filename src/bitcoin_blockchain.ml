open Bitcoin_protocol;;

module Sqlexpr = Sqlexpr_sqlite.Make(Sqlexpr_concurrency.Id);;
module S = Sqlexpr;;

let init_db db = 
  S.execute db
    sqlinit"CREATE TABLE IF NOT EXISTS blockchain(
    id INTEGER PRIMARY KEY,
    hash TEXT COLLATE BINARY NOT NULL,
    height INTEGER NOT NULL,
    previous_block INTEGER NOT NULL
  );";
  S.execute db
    sqlinit"CREATE TABLE IF NOT EXISTS orphans(
    id INTEGER PRIMARY KEY,
    hash TEXT COLLATE BINARY NOT NULL,
    previous_block_hash TEXT COLLATE BINARY NOT NULL
  );";
  S.execute db
    sqlinit"CREATE INDEX IF NOT EXISTS hash_index ON blockchain (hash);";
  S.execute db
    sqlinit"CREATE INDEX IF NOT EXISTS previous_block_index ON blockchain (previous_block);";
;;

let block_hash header =
  let header_bitstring = Bitcoin_protocol_generator.bitstring_of_block_header header in
  Bitcoin_crypto.double_sha256 (Bitstring.string_of_bitstring header_bitstring)
;;

let retrieve_block hash db =
  S.select_one_maybe db
    sqlc"SELECT @L{id}, @s{hash}, @L{height}, @L{previous_block} FROM blockchain WHERE hash LIKE %s" hash
;;

let block_id hash db =
  match retrieve_block hash db with
  | None -> None
  | Some (id, _, _, _) -> Some id
;;
let block_height hash db =
  match retrieve_block hash db with
  | None -> None
  | Some (_, _, height, _) -> Some height
;;

let block_exists hash db =
  match retrieve_block hash db with
  | None -> false
  | Some x -> true
;;

let retrieve_orphan hash db =
  S.select_one_maybe db
    sqlc"SELECT @L{id}, @s{hash}, @s{previous_block_hash} FROM orphans WHERE hash LIKE %s" hash
;;  

let orphan_exists hash db =
  match retrieve_orphan hash db with
  | None -> false
  | Some x -> true
;;

let insert_block_into_blockchain hash previous_block_hash db =
  match block_exists hash db with
  | true -> None
  | false ->
    match retrieve_block previous_block_hash db with
    | None -> None
    | Some (previous_block_id, _, previous_block_height, _) ->
      Some (S.insert db
	      sqlc"INSERT INTO blockchain(hash, height, previous_block) VALUES(%s, %L, %L)"
	      hash
	      (Int64.add previous_block_height 1L)
	      previous_block_id)
;;
let insert_block_as_orphan hash previous_block_hash db =
  match orphan_exists hash db with
  | true -> None
  | false ->
    Some (S.insert db
	    sqlc"INSERT INTO orphans(hash, previous_block_hash) VALUES(%s, %s)"
	    hash
	    previous_block_hash)
;;

let resolve_orphans inserted_hash db =
  let resolve_orphan (id, hash, previous_block_hash) =
    match insert_block_into_blockchain hash previous_block_hash db with
    | Some _ -> 
      S.execute db
	sqlc"DELETE FROM orphans WHERE id = %L"
	id
    | None -> ()
  in
  S.iter db
    resolve_orphan
    sqlc"SELECT @L{id}, @s{hash}, @s{previous_block_hash} FROM orphans WHERE previous_block_hash LIKE %s" inserted_hash
;;
    
let insert_block header db =
  let hash = block_hash header in
  if not (block_exists hash db) then
    if not (block_exists header.previous_block_hash db) then
      (* orphan block *)
      insert_block_as_orphan hash header.previous_block_hash db
    else (
      (* we know the previous block, we can insert this block into the chain *)
      match insert_block_into_blockchain hash header.previous_block_hash db with
      | None -> None
      | Some x as result ->
	(* we inserted a new block into the blockchain, so we should check whether this resolved any dangling orphans *)
	resolve_orphans hash db;
	result
    )
  else
    None
;;

(* we need a special implementation for this, since no previous block exists for the genesis block *)
let insert_genesis_block db =
  let hash = Config.testnet3_genesis_block_hash in
  if not (block_exists Config.testnet3_genesis_block_hash db) then
    Some (S.insert db
	    sqlc"INSERT INTO blockchain(hash, height, previous_block) VALUES(%s, %L, %L)"
	    hash
	    0L
	    0L)
  else
    None
;;
      
let open_db path =
  let db = S.open_db path in
  init_db db;
  ignore (insert_genesis_block db);
  db
;;
