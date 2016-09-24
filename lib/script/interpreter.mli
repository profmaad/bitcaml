open! Core.Std
open Bitcoin_protocol.Std

val execute_script
  :  ?debug:bool
  -> transaction:Transaction.t
  -> input_index:int
  -> Script.t
  -> Data_item.t
