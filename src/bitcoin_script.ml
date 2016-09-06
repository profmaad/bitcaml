open! Core.Std
exception Malformed_script;;

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
  | opcode when (opcode <> 0x50 && (opcode >= 0x00 && opcode <= 0x60)) -> Data (opcode, "")
  | opcode -> InvalidOpcode opcode
;;

let data_item_length = String.length;;
let data_item_compare = String.compare;;
let data_items_equal d1 d2 = (data_item_compare d1 d2 = 0);;
let data_item_byte byte item =
  let index = if byte < 0 then (data_item_length item) + byte else byte in
  Char.to_int item.[index]
;;
let data_item_set_byte byte value item =
  let index = if byte < 0 then (data_item_length item) + byte else byte in
  String.set item index (Char.of_int_exn value)
;;

let int64_of_data_item item =
  let rec process_byte acc byte s =
    if byte >= (data_item_length s) then acc
    else
      let this_byte = if byte = ((data_item_length s) - 1) then (data_item_byte byte s) land (lnot 0x80) else (data_item_byte byte s) in
      let shifted_byte = Int64.shift_left (Int64.of_int this_byte) (byte * 8) in
      let new_acc = Int64.bit_or shifted_byte acc in
      process_byte new_acc (byte + 1) s
  in
  match data_item_length item with
  | 0 -> Some 0L
  | i when (i > 9) -> None
  | _ ->
    let negative = ((data_item_byte (-1) item) land 0x80) > 0 in
    let raw_value = process_byte 0x00L 0 item in
    (* MSB >= 0x80 means negative sign *)
    if negative then (
      (* let magnitude = Int64.bit_and (Int64.shift_left 0x80L (8 * ((data_item_length item) -1 ))) raw_value in *)
      Some (Int64.neg raw_value)
    )
    else
      Some raw_value
;;
let data_item_of_int64 i =
  let rec bytes_of_int64 i =
    if i > 0L then
      let byte = Int64.to_int_exn (Int64.bit_and 0xffL i) in
      let shifted_i = Int64.shift_right_logical i 8 in
      let byte_string = String.make 1 (Char.of_int_exn byte) in
      byte_string ^ (bytes_of_int64 shifted_i)
    else ""
  in
  (* first we must transform the bits of i into sign-magnitude representation *)
  let magnitude = Int64.abs i in
  let negative = (i < 0L) in
  let bytes = bytes_of_int64 magnitude in
  if (data_item_length bytes) = 0 then bytes
  else (
    if ((data_item_byte (-1) bytes) land 0x80) > 0 then
      bytes ^ (String.make 1 (if negative then '\x80' else '\x00'))
    else (
      if negative then (
	let msb = data_item_byte (-1) bytes in
	let signed_msb = msb lor 0x80 in
	data_item_set_byte (-1) signed_msb bytes;
	bytes
      ) else
	bytes
    )
  )
;;

let bool_of_data_item item =
  let char_is_zero c = (c = '\x00') in
  let zero_bytes = Utils.map_string char_is_zero item in
  let all_zero = List.fold ~init:true ~f:( && ) zero_bytes in
  if all_zero then false
  else (
    let zero_bytes_rev = List.rev zero_bytes in
    if (List.fold ~init:true ~f:( && ) (List.tl_exn zero_bytes_rev)) && ((data_item_byte (-1) item) = 0x80) then
      false
    else
      true
  )
;;
let data_item_of_bool = function
  | false -> ""
  | true -> "\x01"
;;

let sigop_count script =
  let constant_pusher_value opcode =
    if (opcode >= 0x51) && (opcode <= 0x60) then opcode - 0x50
    else 20
  in
  let rec sigop_count_acc acc = function
    | [] -> acc
    | CheckSig :: script
    | CheckSigVerify :: script -> sigop_count_acc (acc + 1) script
    | (Data (opcode, _)) :: CheckMultiSig :: script
    | (Data (opcode, _)) :: CheckMultiSigVerify :: script ->
      sigop_count_acc (acc + (constant_pusher_value opcode)) script
    | CheckMultiSig :: script
    | CheckMultiSigVerify :: script -> sigop_count_acc (acc + 20) script
    | _ :: script -> sigop_count_acc acc script
  in
  sigop_count_acc 0 script
;;
