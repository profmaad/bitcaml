open! Core.Std

(** Number of satoshis per btc *)
val coin_size                           : int64

(** Max size of blocks, in bytes *)
val max_block_size                      : int64

(** Number of blocks after which coinbase can be spent *)
val coinbase_maturity                   : int64

(** Max money supply *)
val max_money                           : int64

(** Max time from [now ()] for block timestamps *)
val max_block_into_future               : Time.Span.t

(** Max number of signature operations per block *)
val max_block_signature_operations      : int64

(** Block interval between difficulty adjustments *)
val difficulty_change_interval          : int64

(** Number of previous blocks to check for timestamp verification *)
val timestamp_verification_predecessors : int64

(** Initial reward for mining a block *)
val initial_block_mining_reward         : int64

(** Block interval between reward reductions *)
val block_reward_reduction_interval     : int64

(** Valid lengths for coinbase scripts *)
val valid_coinbase_script_length        : Interval.Int.t
