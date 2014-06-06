open Bitcoin_script;;

let pp_string_of_data_item item =
  match int64_of_data_item item with
  | None -> Utils.hex_string_of_string item
  | Some number_interpretation ->
    Printf.sprintf "%s (as int: %Ld)" (Utils.hex_string_of_string item) number_interpretation
;;

let pp_string_of_word = function
  | Data (_, data_item) -> "Data: " ^ (pp_string_of_data_item data_item)
(* Flow control *)
  | Nop opcode -> Printf.sprintf "NOP: 0x%02x" opcode
  | If -> "OP_IF"
  | NotIf -> "OP_NOTIF"
  | Else -> "OP_ELSE"
  | EndIf -> "OP_ENDIF"
  | Verify -> "OP_VERIFY"
  | Return -> "OP_RETURN"
(* Stack manipulation *)
  | ToAltStack -> "OP_TOALTSTACK"
  | FromAltStack -> "OP_FROMALTSTACK"
  | IfDup -> "OP_IFDUP"
  | Depth -> "OP_DEPTH"
  | Drop -> "OP_DROP"
  | Dup -> "OP_DUP"
  | Nip -> "OP_NIP"
  | Over -> "OP_OVER"
  | Pick -> "OP_PICK"
  | Roll -> "OP_ROLL"
  | Rot -> "OP_ROT"
  | Swap -> "OP_SWAP"
  | Tuck -> "OP_TUCK"
  | TwoDrop -> "OP_2DROP"
  | TwoDup -> "OP_2DUP"
  | ThreeDup -> "OP_3DUP"
  | TwoOver -> "OP_2OVER"
  | TwoRot -> "OP_2ROT"
  | TwoSwap -> "OP_2SWAP"
(* Splice *)
  | Cat -> "OP_CAT (disabled)"
  | Substr -> "OP_SUBSTR (disabled)"
  | Left -> "OP_LEFT (disabled)"
  | Right -> "OP_RIGHT (disabled)"
  | Size -> "OP_SIZE"
(* Bitwise logic *)
  | Invert -> "OP_INVERT (disabled)"
  | And -> "OP_AND (disabled)"
  | Or -> "OP_OR (disabled)"
  | Xor -> "OP_XOR (disabled)"
  | Equal -> "OP_EQUAL"
  | EqualVerify -> "OP_EQUALVERIFY"
(* Arithmetic *)
  | OneAdd -> "OP_1ADD"
  | OneSub -> "OP_1SUB"
  | TwoMul -> "OP_2MUL (disabled)"
  | TwoDiv -> "OP_2DIV (disabled)"
  | Negate -> "OP_NEGATE"
  | Abs -> "OP_ABS"
  | Not -> "OP_NOT"
  | ZeroNotEqual -> "OP_0NOTEQUAL"
  | Add -> "OP_ADD"
  | Sub -> "OP_SUB"
  | Mul -> "OP_MUL (disabled)"
  | Div -> "OP_DIV (disabled)"
  | Mod -> "OP_MOD (disabled)"
  | LShift -> "OP_LSHIFT (disabled)"
  | RShift -> "OP_RSHIFT (disabled)"
  | BoolAnd -> "OP_BOOlAND"
  | BoolOr -> "OP_BOOLOR"
  | NumEqual -> "OP_NUMEQUAL"
  | NumEqualVerify -> "OP_NUMEQUALVERIFY"
  | NumNotEqual -> "OP_NUMNOTEQUAL"
  | LessThan -> "OP_LESSTHAN"
  | GreaterThan -> "OP_GREATERTHAN"
  | LessThanOrEqual -> "OP_LESSTHENOREQUAL"
  | GreaterThanOrEqual -> "OP_GREATERTHANOREQUAL"
  | Min -> "OP_MIN"
  | Max -> "OP_MAX"
  | Within -> "OP_WITHIN"
(* Cryptography *)
  | RIPEMD160 -> "OP_RIPEMD160"
  | SHA1 -> "OP_SHA1"
  | SHA256 -> "OP_SHA256"
  | Hash160 -> "OP_HASH160"
  | Hash256 -> "OP_HASH256"
  | CodeSeparator -> "OP_CODESEPARATOR"
  | CheckSig -> "OP_CHECKSIG"
  | CheckSigVerify -> "OP_CHECKSIGVERIFY"
  | CheckMultiSig -> "OP_CHECKMULTISIG"
  | CheckMultiSigVerify -> "OP_CHECKMULTISIGVERIFY"
(* Pseudo-words *)
  | PubKeyHash -> "OP_PUBKEYHASH"
  | PubKey -> "OP_PUBKEY"
  | InvalidOpcode opcode -> Printf.sprintf "OP_INVALIDOPCODE: 0x%02x" opcode
(* Reserved *)
  | Reserved -> "OP_RESERVED"
  | Ver -> "OP_VER"
  | VerIf -> "OP_VERIF"
  | VerNotIf -> "OP_VERNOTIF"
  | Reserved1 -> "OP_RESERVED1"
  | Reserved2 -> "OP_RESERVED2"
;;

let print_script script =
  let print_word_with_index index word =
    Printf.printf "\t%u:\t%s\n" (index + 1) (pp_string_of_word word)
  in
  Printf.printf "Bitcoin Transaction Script (%u words):\n" (List.length script);
  List.iteri print_word_with_index script
;;
