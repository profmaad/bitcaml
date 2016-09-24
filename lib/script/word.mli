open! Core.Std

type t =
  | Data          of {opcode : int; data : Data_item.t}
  | Nop           of int
  | InvalidOpcode of int
  | If
  | NotIf
  | Else
  | EndIf
  | Verify
  | Return
  | ToAltStack
  | FromAltStack
  | IfDup
  | Depth
  | Drop
  | Dup
  | Nip
  | Over
  | Pick
  | Roll
  | Rot
  | Swap
  | Tuck
  | TwoDrop
  | TwoDup
  | ThreeDup
  | TwoOver
  | TwoRot
  | TwoSwap
  | Cat
  | Substr
  | Left
  | Right
  | Size
  | Invert
  | And
  | Or
  | Xor
  | Equal
  | EqualVerify
  | OneAdd
  | OneSub
  | TwoMul
  | TwoDiv
  | Negate
  | Abs
  | Not
  | ZeroNotEqual
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | LShift
  | RShift
  | BoolAnd
  | BoolOr
  | NumEqual
  | NumEqualVerify
  | NumNotEqual
  | LessThan
  | GreaterThan
  | LessThanOrEqual
  | GreaterThanOrEqual
  | Min
  | Max
  | Within
  | RIPEMD160
  | SHA1
  | SHA256
  | Hash160
  | Hash256
  | CodeSeparator
  | CheckSig
  | CheckSigVerify
  | CheckMultiSig
  | CheckMultiSigVerify
  | PubKeyHash
  | PubKey
  | Reserved
  | Ver
  | VerIf
  | VerNotIf
  | Reserved1
  | Reserved2
[@@deriving bin_io, compare, sexp]

include Bitcaml_utils.Std.Bitstringable with type t := t

val to_string_hum : t -> string

val to_opcode : t -> int
val of_opcode : int -> t
