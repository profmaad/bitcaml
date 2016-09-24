open! Core.Std
open Bitcoin_protocol.Std

module Protocol_block = Block

type db
type t = db

module Record_id : sig
  type t [@@deriving compare, sexp_of]
end

module InsertionResult : sig
  type t =
    | InsertedIntoBlockchain of Record_id.t
    | InsertedAsOrphan       of Record_id.t
    | Existed
    | InsertionFailed
  [@@deriving compare, sexp_of]
end

module Orphan : sig
  type t [@@deriving compare, sexp_of]

  val create
    :  id:Record_id.t
    -> hash:string
    -> previous_block_hash:string
    -> log_difficulty:float
    -> header:Protocol_block.Header.t
    -> t

    val id                  : t -> Record_id.t
    val hash                : t -> string
    val previous_block_hash : t -> string
    val log_difficulty      : t -> float
    val header              : t -> Protocol_block.Header.t

    val by_id                  : db -> Record_id.t  -> t
    val by_hash                : db -> string       -> t
    val by_previous_block_hash : db -> string       -> t list

    val hash_exists : db -> string -> bool

    val insert : db -> t -> Record_id.t
    val delete : db -> Record_id.t -> unit
end

module Block : sig
  type t [@@deriving compare, sexp_of]

  val create
    :  id:Record_id.t
    -> hash:string
    -> height:int64
    -> cumulative_difficulty:float
    -> previous_block_id:Record_id.t
    -> is_main:bool
    -> header:Block.Header.t
    -> t

    val id                    : t -> Record_id.t
    val hash                  : t -> string
    val height                : t -> int64
    val cumulative_difficulty : t -> float
    val previous_block_id     : t -> Record_id.t
    val is_main               : t -> bool
    val header                : t -> Protocol_block.Header.t

    val by_id   : db -> Record_id.t -> t
    val by_hash : db -> string      -> t

    val mainchain_tip       : db          -> t
    val mainchain_at_height : db -> int64 -> t

    val hash_exists : db -> string -> bool

    val insert : db -> t -> Record_id.t
end

module UnspentTransactionOutput : sig
  type t [@@deriving compare, sexp_of]

  val create
    :  id:Record_id.t
    -> hash:string
    -> output_index:int32
    -> block_id:Record_id.t
    -> value:int64
    -> script:string
    -> is_coinbase:bool
    -> t

  val id           : t -> Record_id.t
  val hash         : t -> string
  val output_index : t -> int32
  val block_id     : t -> Record_id.t
  val value        : t -> int64
  val script       : t -> string
  val is_coinbase  : t -> bool

  val by_hash_and_index : db -> string -> int32 -> t

  val delete_by_hash           : db -> string          -> unit
  val delete_by_hash_and_index : db -> string -> int32 -> unit

  val insert : db -> t -> Record_id.t
end

module MemoryPool : sig
  type t [@@deriving compare, sexp_of]

  val create
    :  id:Record_id.t
    -> hash:string
    -> output_count:int32
    -> is_orphan:bool
    -> data:string
    -> t

  val id           : t -> Record_id.t
  val hash         : t -> string
  val output_count : t -> int32
  val is_orphan    : t -> bool
  val data         : t -> string

  val by_hash : db -> string -> t

  val hash_exists : db -> string -> bool

  val delete_by_hash : db -> string -> unit
end

val mainchain_block_id_and_index_for_transaction_hash
  :  db
  -> string
  -> (Record_id.t * int) option

val mainchain_block_hash_and_index_for_transaction_hash
  :  db
  -> string
  -> (string * int) option

val mainchain_block_id_hash_and_index_for_transaction_hash
  :  db
  -> string
  -> (Record_id.t * string * int) option

val mainchain_transaction_hash_exists
  :  db
  -> string
  -> bool

val nth_predecessor
  :  db
  -> string
  -> int64
  -> Block.t

val retrieve_n_predecessors
  :  db
  -> string
  -> int
  -> Block.t list

val retrieve_sidechain_with_leaf
  :  db
  -> string
  -> Block.t list * Block.t

val retrieve_between_hashes
  :  db
  -> leaf_hash:string
  -> base_hash:string
  -> Block.t list

val rollback_mainchain_to_height
  :  db
  -> int64
  -> unit

val block_exists_anywhere
  :  db
  -> string
  -> bool

val delete_block_transactions_from_mempool
  :  db
  -> Protocol_block.t
  -> unit

val update_utxo_with_block
  :  db
  -> Protocol_block.t
  -> string
  -> unit

val register_transactions_for_block
  :  db
  -> Protocol_block.t
  -> Record_id.t
  -> unit

val insert_block_into_blockchain
  :  db
  -> hash:string
  -> previous_block_hash:string
  -> log_difficulty:float
  -> Protocol_block.Header.t
  -> InsertionResult.t

val insert_block_as_orphan
  :  db
  -> hash:string
  -> previous_block_hash:string
  -> log_difficulty:float
  -> Protocol_block.Header.t
  -> InsertionResult.t

val open_db
  :  ?network:Magic.t
  -> file:string
  -> unit
  -> t
