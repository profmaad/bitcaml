open! Core.Std

let coin_size                           = 100000000L

let max_block_size                      = 1000000L

let coinbase_maturity                   = 100L

let max_money                           = Int64.( * ) 21000000L coin_size

let max_block_into_future               = Time.Span.of_hr 2.

let max_block_signature_operations      = Int64.(/) max_block_size 50L

let difficulty_change_interval          = 2016L

let timestamp_verification_predecessors = 1L

let initial_block_mining_reward         = Int64.( * ) coin_size 50L

let block_reward_reduction_interval     = 210000L

let valid_coinbase_script_length        = Interval.Int.create 2 100
