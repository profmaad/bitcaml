open! Core.Std
open Bitcoin_crypto.Std
open Bitcaml_utils.Std

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

let to_opcode = function
  | Data {opcode; data = _} -> opcode
  | Nop opcode              -> opcode
  | InvalidOpcode opcode    -> opcode
  | Reserved                -> 0x50
  | Ver                     -> 0x62
  | If                      -> 0x63
  | NotIf                   -> 0x64
  | VerIf                   -> 0x65
  | VerNotIf                -> 0x66
  | Else                    -> 0x67
  | EndIf                   -> 0x68
  | Verify                  -> 0x69
  | Return                  -> 0x6a
  | ToAltStack              -> 0x6b
  | FromAltStack            -> 0x6c
  | IfDup                   -> 0x73
  | Depth                   -> 0x74
  | Drop                    -> 0x75
  | Dup                     -> 0x76
  | Nip                     -> 0x77
  | Over                    -> 0x78
  | Pick                    -> 0x79
  | Roll                    -> 0x7a
  | Rot                     -> 0x7b
  | Swap                    -> 0x7c
  | Tuck                    -> 0x7d
  | TwoDrop                 -> 0x6d
  | TwoDup                  -> 0x6e
  | ThreeDup                -> 0x6f
  | TwoOver                 -> 0x70
  | TwoRot                  -> 0x71
  | TwoSwap                 -> 0x72
  | Cat                     -> 0x7e
  | Substr                  -> 0x7f
  | Left                    -> 0x80
  | Right                   -> 0x81
  | Size                    -> 0x82
  | Invert                  -> 0x83
  | And                     -> 0x84
  | Or                      -> 0x85
  | Xor                     -> 0x86
  | Equal                   -> 0x87
  | EqualVerify             -> 0x88
  | Reserved1               -> 0x89
  | Reserved2               -> 0x8a
  | OneAdd                  -> 0x8b
  | OneSub                  -> 0x8c
  | TwoMul                  -> 0x8d
  | TwoDiv                  -> 0x8e
  | Negate                  -> 0x8f
  | Abs                     -> 0x90
  | Not                     -> 0x91
  | ZeroNotEqual            -> 0x92
  | Add                     -> 0x93
  | Sub                     -> 0x94
  | Mul                     -> 0x95
  | Div                     -> 0x96
  | Mod                     -> 0x97
  | LShift                  -> 0x98
  | RShift                  -> 0x99
  | BoolAnd                 -> 0x9a
  | BoolOr                  -> 0x9b
  | NumEqual                -> 0x9c
  | NumEqualVerify          -> 0x9d
  | NumNotEqual             -> 0x9e
  | LessThan                -> 0x9f
  | GreaterThan             -> 0xa0
  | LessThanOrEqual         -> 0xa1
  | GreaterThanOrEqual      -> 0xa2
  | Min                     -> 0xa3
  | Max                     -> 0xa4
  | Within                  -> 0xa5
  | RIPEMD160               -> 0xa6
  | SHA1                    -> 0xa7
  | SHA256                  -> 0xa8
  | Hash160                 -> 0xa9
  | Hash256                 -> 0xaa
  | CodeSeparator           -> 0xab
  | CheckSig                -> 0xac
  | CheckSigVerify          -> 0xad
  | CheckMultiSig           -> 0xae
  | CheckMultiSigVerify     -> 0xaf
  | PubKeyHash              -> 0xfd
  | PubKey                  -> 0xfe
;;

let of_opcode = function
  | 0x50 -> Reserved
  | 0x62 -> Ver
  | 0x63 -> If
  | 0x64 -> NotIf
  | 0x65 -> VerIf
  | 0x66 -> VerNotIf
  | 0x67 -> Else
  | 0x68 -> EndIf
  | 0x69 -> Verify
  | 0x6a -> Return
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
  | 0x7e -> Cat
  | 0x7f -> Substr
  | 0x80 -> Left
  | 0x81 -> Right
  | 0x82 -> Size
  | 0x83 -> Invert
  | 0x84 -> And
  | 0x85 -> Or
  | 0x86 -> Xor
  | 0x87 -> Equal
  | 0x88 -> EqualVerify
  | 0x89 -> Reserved1
  | 0x8a -> Reserved2
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
  | 0xfd -> PubKeyHash
  | 0xfe -> PubKey
  | 0xff -> InvalidOpcode 0xff
  | opcode when (opcode = 0x61 || (opcode >= 0xb0 && opcode <= 0xb9)) ->
    Nop opcode
  | opcode when (opcode <> 0x50 && (opcode >= 0x00 && opcode <= 0x60)) ->
    Data {opcode; data = Data_item.of_string ""}
  | opcode ->
    InvalidOpcode opcode
;;

let data_of_bitstring bits = function
  | 0x00 -> Data {opcode = 0x00; data = ""}, bits
  | 0x4f -> Data {opcode = 0x00; data = "\x81"}, bits
  | opcode when (opcode >= 0x51 && opcode <= 0x60) ->
    let data =
      Char.of_int_exn (opcode - 0x50)
      |> String.of_char
    in
    Data {opcode; data}, bits
  | opcode when (opcode >= 0x01 && opcode <= 0x4b) ->
    let data =
      Bitstring.takebits (opcode * 8) bits
      |> Bitstring.string_of_bitstring
    in
    let bits = Bitstring.dropbits (opcode * 8) bits in
    Data {opcode; data}, bits
  | opcode when (opcode >= 0x4c && opcode <= 0x4e) ->
    let int_length =
      match opcode with
      | 0x4c -> 1
      | 0x4d -> 2
      | 0x4e -> 4
      | _    -> failwith "BUG: this match case can't be reached"
    in
    let length, bits =
      match%bitstring bits with
      | {| length : int_length * 8 : littleendian
         ; bits   :             -1 : bitstring
        |} ->
        Int64.to_int_exn length * 8, bits
      | {| _ |} -> failwith "invalid data word"
    in
    let data =
      Bitstring.takebits length bits
      |> Bitstring.string_of_bitstring
    in
    let bits = Bitstring.dropbits length bits in
    Data {opcode; data}, bits
  | _ -> failwith "invalid data word"
;;

let of_bitstring = function%bitstring
  | {| opcode : 1*8 : littleendian
     ; bits   :  -1 : bitstring
    |} ->
    begin match of_opcode opcode with
    | Data {opcode; data = _} -> data_of_bitstring bits opcode
    | t                       -> t, bits
    end
  | {| _ |} -> failwith "invalid word"

;;

let data_to_bitstring opcode data =
  let length = Data_item.length data in
  let payload =
    match opcode with
    | 0x00
    | 0x4f ->
      Bitstring.empty_bitstring
    | opcode when (opcode >= 0x51 && opcode <= 0x60) ->
      Bitstring.empty_bitstring
    | opcode when (opcode >= 0x01 && opcode <= 0x4b) ->
      if Int.equal length opcode then
        Data_item.to_bitstring data
      else
        failwith "malformed data word"
    | 0x4c ->
      if Int.(<=) length 0xff then
        let data = Data_item.to_bitstring data in
        [%bitstring
          {| length : 1*8 : littleendian
           ; data   :  -1 : bitstring
          |}]
      else
        failwith "malformed data word"
    | 0x4d ->
      if Int.(<=) length 0xffff then
        let data = Data_item.to_bitstring data in
        [%bitstring
          {| length : 2*8 : littleendian
           ; data   :  -1 : bitstring
          |}]
      else
        failwith "malformed data word"
    | 0x4e ->
      if Int.(<=) length 0xffffffff then
        let length = Int32.of_int_exn length in
        let data = Data_item.to_bitstring data in
        [%bitstring
          {| length : 4*8 : littleendian
           ; data   :  -1 : bitstring
          |}]
      else
        failwith "malformed data word"
    | _ -> failwith "malformed data word"
  in
  [%bitstring
    {| opcode  : 1*8 : littleendian
     ; payload :  -1 : bitstring
    |}]
;;

let to_bitstring = function
  | Data {opcode; data} ->
    data_to_bitstring opcode data
  | word ->
    let opcode = to_opcode word in
    [%bitstring {| opcode : 1*8 : littleendian |}]
;;

let to_string_hum = function
  | Data {data; opcode = _} -> sprintf !"Data: '%{Hash_string#hum}'" (Hash_string.of_bytes (Data_item.to_string data))
  | Nop opcode              -> sprintf "NOP: 0x%02x" opcode
  | InvalidOpcode opcode    -> sprintf "OP_INVALIDOPCODE: 0x%02x" opcode
  | If                  -> "OP_IF"
  | NotIf               -> "OP_NOTIF"
  | Else                -> "OP_ELSE"
  | EndIf               -> "OP_ENDIF"
  | Verify              -> "OP_VERIFY"
  | Return              -> "OP_RETURN"
  | ToAltStack          -> "OP_TOALTSTACK"
  | FromAltStack        -> "OP_FROMALTSTACK"
  | IfDup               -> "OP_IFDUP"
  | Depth               -> "OP_DEPTH"
  | Drop                -> "OP_DROP"
  | Dup                 -> "OP_DUP"
  | Nip                 -> "OP_NIP"
  | Over                -> "OP_OVER"
  | Pick                -> "OP_PICK"
  | Roll                -> "OP_ROLL"
  | Rot                 -> "OP_ROT"
  | Swap                -> "OP_SWAP"
  | Tuck                -> "OP_TUCK"
  | TwoDrop             -> "OP_2DROP"
  | TwoDup              -> "OP_2DUP"
  | ThreeDup            -> "OP_3DUP"
  | TwoOver             -> "OP_2OVER"
  | TwoRot              -> "OP_2ROT"
  | TwoSwap             -> "OP_2SWAP"
  | Cat                 -> "OP_CAT (disabled)"
  | Substr              -> "OP_SUBSTR (disabled)"
  | Left                -> "OP_LEFT (disabled)"
  | Right               -> "OP_RIGHT (disabled)"
  | Size                -> "OP_SIZE"
  | Invert              -> "OP_INVERT (disabled)"
  | And                 -> "OP_AND (disabled)"
  | Or                  -> "OP_OR (disabled)"
  | Xor                 -> "OP_XOR (disabled)"
  | Equal               -> "OP_EQUAL"
  | EqualVerify         -> "OP_EQUALVERIFY"
  | OneAdd              -> "OP_1ADD"
  | OneSub              -> "OP_1SUB"
  | TwoMul              -> "OP_2MUL (disabled)"
  | TwoDiv              -> "OP_2DIV (disabled)"
  | Negate              -> "OP_NEGATE"
  | Abs                 -> "OP_ABS"
  | Not                 -> "OP_NOT"
  | ZeroNotEqual        -> "OP_0NOTEQUAL"
  | Add                 -> "OP_ADD"
  | Sub                 -> "OP_SUB"
  | Mul                 -> "OP_MUL (disabled)"
  | Div                 -> "OP_DIV (disabled)"
  | Mod                 -> "OP_MOD (disabled)"
  | LShift              -> "OP_LSHIFT (disabled)"
  | RShift              -> "OP_RSHIFT (disabled)"
  | BoolAnd             -> "OP_BOOlAND"
  | BoolOr              -> "OP_BOOLOR"
  | NumEqual            -> "OP_NUMEQUAL"
  | NumEqualVerify      -> "OP_NUMEQUALVERIFY"
  | NumNotEqual         -> "OP_NUMNOTEQUAL"
  | LessThan            -> "OP_LESSTHAN"
  | GreaterThan         -> "OP_GREATERTHAN"
  | LessThanOrEqual     -> "OP_LESSTHENOREQUAL"
  | GreaterThanOrEqual  -> "OP_GREATERTHANOREQUAL"
  | Min                 -> "OP_MIN"
  | Max                 -> "OP_MAX"
  | Within              -> "OP_WITHIN"
  | RIPEMD160           -> "OP_RIPEMD160"
  | SHA1                -> "OP_SHA1"
  | SHA256              -> "OP_SHA256"
  | Hash160             -> "OP_HASH160"
  | Hash256             -> "OP_HASH256"
  | CodeSeparator       -> "OP_CODESEPARATOR"
  | CheckSig            -> "OP_CHECKSIG"
  | CheckSigVerify      -> "OP_CHECKSIGVERIFY"
  | CheckMultiSig       -> "OP_CHECKMULTISIG"
  | CheckMultiSigVerify -> "OP_CHECKMULTISIGVERIFY"
  | PubKeyHash          -> "OP_PUBKEYHASH"
  | PubKey              -> "OP_PUBKEY"
  | Reserved            -> "OP_RESERVED"
  | Ver                 -> "OP_VER"
  | VerIf               -> "OP_VERIF"
  | VerNotIf            -> "OP_VERNOTIF"
  | Reserved1           -> "OP_RESERVED1"
  | Reserved2           -> "OP_RESERVED2"
;;
