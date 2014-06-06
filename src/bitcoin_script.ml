type data_item = string;;

type word =
(* Data represents any of the "constant" words. The int holds the original opcode, the string is the data as a binary string *)
| Data of int * data_item
(* Flow control *)
| Nop of int
| If
| NotIf
| Else
| EndIf
| Verify
| Return
(* Stack manipulation *)
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
(* Splice *)
| Cat
| Substr
| Left
| Right
| Size
(* Bitwise logic *)
| Invert
| And
| Or
| Xor
| Equal
| EqualVerify
(* Arithmetic *)
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
(* Cryptography *)
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
(* Pseudo-words *)
| PubKeyHash
| PubKey
| InvalidOpcode of int
(* Reserved *)
| Reserved
| Ver
| VerIf
| VerNotIf
| Reserved1
| Reserved2
;;

type script = word list;;

let opcode_of_word = function
  | Data (opcode, _) -> opcode
(* Flow control *)
  | Nop opcode -> opcode
  | If -> 0x63
  | NotIf -> 0x64
  | Else -> 0x67
  | EndIf -> 0x68
  | Verify -> 0x69
  | Return -> 0x6a
(* Stack manipulation *)
  | ToAltStack -> 0x6b
  | FromAltStack -> 0x6c
  | IfDup -> 0x73
  | Depth -> 0x74
  | Drop -> 0x75
  | Dup -> 0x76
  | Nip -> 0x77
  | Over -> 0x78
  | Pick -> 0x79
  | Roll -> 0x7a
  | Rot -> 0x7b
  | Swap -> 0x7c
  | Tuck -> 0x7d
  | TwoDrop -> 0x6d
  | TwoDup -> 0x6e
  | ThreeDup -> 0x6f
  | TwoOver -> 0x70
  | TwoRot -> 0x71
  | TwoSwap -> 0x72
(* Splice *)
  | Cat -> 0x7e
  | Substr -> 0x7f
  | Left -> 0x80
  | Right -> 0x81
  | Size -> 0x82
(* Bitwise logic *)
  | Invert -> 0x83
  | And -> 0x84
  | Or -> 0x85
  | Xor -> 0x86
  | Equal -> 0x87
  | EqualVerify -> 0x88
(* Arithmetic *)
  | OneAdd -> 0x8b
  | OneSub -> 0x8c
  | TwoMul -> 0x8d
  | TwoDiv -> 0x8e
  | Negate -> 0x8f
  | Abs -> 0x90
  | Not -> 0x91
  | ZeroNotEqual -> 0x92
  | Add -> 0x93
  | Sub -> 0x94
  | Mul -> 0x95
  | Div -> 0x96
  | Mod -> 0x97
  | LShift -> 0x98
  | RShift -> 0x99
  | BoolAnd -> 0x9a
  | BoolOr -> 0x9b
  | NumEqual -> 0x9c
  | NumEqualVerify -> 0x9d
  | NumNotEqual -> 0x9e
  | LessThan -> 0x9f
  | GreaterThan -> 0xa0
  | LessThanOrEqual -> 0xa1
  | GreaterThanOrEqual -> 0xa2
  | Min -> 0xa3
  | Max -> 0xa4
  | Within -> 0xa5
(* Cryptography *)
  | RIPEMD160 -> 0xa6
  | SHA1 -> 0xa7
  | SHA256 -> 0xa8
  | Hash160 -> 0xa9
  | Hash256 -> 0xaa
  | CodeSeparator -> 0xab
  | CheckSig -> 0xac
  | CheckSigVerify -> 0xad
  | CheckMultiSig -> 0xae
  | CheckMultiSigVerify -> 0xaf
(* Pseudo-words *)
  | PubKeyHash -> 0xfd
  | PubKey -> 0xfe
  | InvalidOpcode opcode -> opcode
(* Reserved *)
  | Reserved -> 0x50
  | Ver -> 0x62
  | VerIf -> 0x65
  | VerNotIf -> 0x66
  | Reserved1 -> 0x89
  | Reserved2 -> 0x8a
;;
let word_of_opcode = function
(* Flow control *)
  | 0x63 -> If
  | 0x64 -> NotIf
  | 0x67 -> Else
  | 0x68 -> EndIf
  | 0x69 -> Verify
  | 0x6a -> Return
(* Stack manipulation *)
  | 0x6b -> ToAltStack
  | 0x6c -> FromAltStack
  | 0x73 -> IfDup
  | 0x74 -> Depth
  | 0x75 -> Drop
  | 0x76 -> Dup
  | 0x77 -> Nip
  | 0x78 -> Over
  | 0x79 -> Pick
  | 0x7a -> Roll
  | 0x7b -> Rot
  | 0x7c -> Swap
  | 0x7d -> Tuck
  | 0x6d -> TwoDrop
  | 0x6e -> TwoDup
  | 0x6f -> ThreeDup
  | 0x70 -> TwoOver
  | 0x71 -> TwoRot
  | 0x72 -> TwoSwap
(* Splice *)
  | 0x7e -> Cat
  | 0x7f -> Substr
  | 0x80 -> Left
  | 0x81 -> Right
  | 0x82 -> Size
(* Bitwise logic *)
  | 0x83 -> Invert
  | 0x84 -> And
  | 0x85 -> Or
  | 0x86 -> Xor
  | 0x87 -> Equal
  | 0x88 -> EqualVerify
(* Arithmetic *)
  | 0x8b -> OneAdd
  | 0x8c -> OneSub
  | 0x8d -> TwoMul
  | 0x8e -> TwoDiv
  | 0x8f -> Negate
  | 0x90 -> Abs
  | 0x91 -> Not
  | 0x92 -> ZeroNotEqual
  | 0x93 -> Add
  | 0x94 -> Sub
  | 0x95 -> Mul
  | 0x96 -> Div
  | 0x97 -> Mod
  | 0x98 -> LShift
  | 0x99 -> RShift
  | 0x9a -> BoolAnd
  | 0x9b -> BoolOr
  | 0x9c -> NumEqual
  | 0x9d -> NumEqualVerify
  | 0x9e -> NumNotEqual
  | 0x9f -> LessThan
  | 0xa0 -> GreaterThan
  | 0xa1 -> LessThanOrEqual
  | 0xa2 -> GreaterThanOrEqual
  | 0xa3 -> Min
  | 0xa4 -> Max
  | 0xa5 -> Within
(* Cryptography *)
  | 0xa6 -> RIPEMD160
  | 0xa7 -> SHA1
  | 0xa8 -> SHA256
  | 0xa9 -> Hash160
  | 0xaa -> Hash256
  | 0xab -> CodeSeparator
  | 0xac -> CheckSig
  | 0xad -> CheckSigVerify
  | 0xae -> CheckMultiSig
  | 0xaf -> CheckMultiSigVerify
(* Pseudo-words *)
  | 0xfd -> PubKeyHash
  | 0xfe -> PubKey
  | 0xff -> InvalidOpcode 0xff
(* Reserved *)
  | 0x50 -> Reserved
  | 0x62 -> Ver
  | 0x65 -> VerIf
  | 0x66 -> VerNotIf
  | 0x89 -> Reserved1
  | 0x8a -> Reserved2
(* for these two, we match a set of opcodes to the same word *)
  | opcode when (opcode = 0x61 || (opcode >=0xb0 && opcode <= 0xb9)) -> Nop opcode
  | opcode when (opcode != 0x50 && (opcode >= 0x00 && opcode <= 0x60)) -> Data (opcode, "")
  | opcode -> InvalidOpcode opcode
;;

let int64_of_data_item item =
  match String.length item with
  | 0 -> Some 0L
  | i when (i != 1 && i != 3 && i != 5 && i != 9) -> None
  | _ ->
    match Bitcoin_protocol_parser.parse_varint (Bitstring.bitstring_of_string item) with
    | None, _ -> None
    | Some int64_value, _ ->
      let magnitude = Int64.logand 0x7fffffffffffffffL int64_value in
      let negative = (Int64.logand 0x8000000000000000L int64_value) != 0L in
      Some (if negative then Int64.neg magnitude else magnitude)
;;