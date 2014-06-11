open Bitcoin_protocol;;
open Bitcoin_script;;

exception Disabled_opcode;;
exception Result_invalid;;
exception Illegal_arithmetic;;
exception Not_implemented;;

let max_arithmetic_data_item_size = 4;;

type stack = data_item Stack.t;;

type result =
| Result of data_item
| Invalid

type hash_type =
| SigHashAllZero (* bug backwards compatibility *)
| SigHashAll
| SigHashNone
| SigHashSingle
| UnknownHashType of int
;;
type hash_type_flags =
| SigHashAnyoneCanPay

let hash_type_of_int i =
  match (i land 0x0f) with
  | 0x00 -> SigHashAllZero
  | 0x01 -> SigHashAll
  | 0x02 -> SigHashNone
  | 0x03 -> SigHashSingle
  | _ -> UnknownHashType i
;;
let int_of_hash_type = function
  | SigHashAllZero -> 0x00
  | SigHashAll -> 0x01
  | SigHashNone -> 0x02
  | SigHashSingle -> 0x03
  | UnknownHashType i -> i
;;

let hash_type_flags_of_int i =
  let flags = ref [] in
  if (i land 0x80) > 0 then flags := SigHashAnyoneCanPay :: !flags;
  !flags
;;
let int_of_hash_type_flags flags =
  let int_of_hash_type_flag = function
    | SigHashAnyoneCanPay -> 0x80
  in
  List.fold_left ( lor ) 0x00 (List.map int_of_hash_type_flag flags)
;;

let hash_type_and_flags_of_int i =
  (hash_type_of_int i, hash_type_flags_of_int i)
;;
let int_of_hash_type_and_flags hash_type flags =
  (int_of_hash_type hash_type) lor (int_of_hash_type_flags flags)
;;

let create_stack () =
  let (stack : stack) = Stack.create () in
  stack
;;
let push (data_item : data_item) (stack : stack) = Stack.push data_item stack;;
let pop (stack : stack) =
  let (top_item : data_item) = Stack.pop stack in
  top_item
;;
let top (stack : stack) =
  let (top_item : data_item) = Stack.top stack in
  top_item
;;

let pop_to_list n stack =
  let rec pop_acc acc n stack =
    if n = 0 then acc
    else pop_acc ((pop stack) :: acc) (n - 1) stack
  in
  pop_acc [] n stack
;;
let push_list items stack =
  List.iter (fun item -> push item stack) items
;;

let pop_n n (stack : stack) =
  let popped_items = pop_to_list (n - 1) stack in
  let item = pop stack in
  push_list popped_items stack;
  item
;;
let top_n n (stack : stack) =
  let popped_items = pop_to_list (n - 1) stack in
  let item = top stack in
  push_list popped_items stack;
  item
;;

let roll_n n stack =
  let item = pop_n n stack in
  push item stack
;;  
let dup_n n stack =
  let items = pop_to_list n stack in
  push_list items stack;
  push_list items stack
;;

let op_nip stack =
  let first = pop stack in
  ignore (pop stack);
  push first stack
;;
let op_over stack =
  let first = pop stack in
  let second = top stack in
  push first stack;
  push second stack
;;
let op_pick stack =
  match int64_of_data_item (pop stack) with
  | None -> raise Result_invalid
  | Some n -> 
    let n = Int64.to_int n in
    let item = top_n n stack in
    push item stack
;;
let op_roll stack =
  match int64_of_data_item (pop stack) with
  | None -> raise Result_invalid
  | Some n ->
    let n = Int64.to_int n in
    roll_n n stack
;;
let op_tuck stack =
  let first = pop stack in
  let second = pop stack in
  push_list [first; second; first] stack
;;
let op_2over stack =
  let first = pop_to_list 2 stack in
  let second = pop_to_list 2 stack in
  push_list second stack;
  push_list first stack;
  push_list second stack
;;
let op_2rot stack =
  let first = pop_to_list 2 stack in
  let second = pop_to_list 2 stack in
  let third = pop_to_list 2 stack in
  push_list second stack;
  push_list first stack;
  push_list third stack
;;
let op_2swap stack =
  let first = pop_to_list 2 stack in
  let second = pop_to_list 2 stack in
  push_list first stack;
  push_list second stack
;;

let op_size stack =
  let item = top stack in
  let length = Int64.of_int (data_item_length item) in
  push (data_item_of_int64 length) stack
;;

let op_verify stack =
  if bool_of_data_item (top stack) then
    ignore (pop stack)
  else
    raise Result_invalid
;;

let op_equal stack =
  push (data_item_of_bool (data_items_equal (pop stack) (pop stack))) stack
;;

let int32_of_data_item item =
  if data_item_length item > max_arithmetic_data_item_size then raise Illegal_arithmetic
  else
    match int64_of_data_item item with
    | None -> raise Illegal_arithmetic
    | Some i -> Int64.to_int32 i
;;
let push_int32 i (stack : stack) = Stack.push (data_item_of_int64 (Int64.of_int32 i)) stack;;
let pop_int32 (stack : stack) =
  let (top_item : data_item) = Stack.pop stack in
  int32_of_data_item top_item
;;

let bool_of_int32 = function
  | 0l -> false
  | _ -> true
;;
let int32_of_bool = function
  | false -> 0l
  | true -> 1l
;;

let push_bool_arithmetic b (stack : stack) = push_int32 (int32_of_bool b) stack;;
let pop_bool_arithmetic (stack : stack) = bool_of_int32 (pop_int32 stack);;

let op_not stack = push_bool_arithmetic (not (pop_bool_arithmetic stack)) stack;;
let op_0notequal stack =
  let bool_item = bool_of_int32 (pop_int32 stack) in
  push_int32 (int32_of_bool bool_item) stack
;;
let op_sub stack =
  let b = pop_int32 stack in
  let a = pop_int32 stack in
  push_int32 (Int32.sub a b) stack
;; 
let minmax f stack =
  let b = pop_int32 stack in
  let a = pop_int32 stack in
  push_int32 (if (f a b) then a else b) stack
  
let op_min stack = minmax ( < ) stack;;
let op_max stack = minmax ( > ) stack;;
let op_within stack = 
  let max = pop_int32 stack in
  let min = pop_int32 stack in
  let x = pop_int32 stack in
  push_bool_arithmetic (min <= x && x < max) stack
;;

let hash f stack =
  push (f (pop stack)) stack
;;

let checksig_copy_tx tx input_index subscript =
  let set_scripts index txin =
    if index = input_index then
      { txin with signature_script = subscript }
    else
      { txin with signature_script = "" }
  in
  let modified_inputs = List.mapi set_scripts tx.transaction_inputs in
  { tx with transaction_inputs = modified_inputs }
;;
let zero_input_sequence_numbers inputs input_index =
  let set_sequence_numbers index txin =
    if index = input_index then txin
    else
      { txin with transaction_sequence_number = 0x0000l }
  in
  List.mapi set_sequence_numbers inputs
;;
let checksig_copy_tx_sighashnone tx input_index =
  let modified_inputs = zero_input_sequence_numbers tx.transaction_inputs input_index in
  { tx with
    transaction_inputs = modified_inputs;
    transaction_outputs = [];
  }
;;
let checksig_copy_tx_sighashsingle tx input_index =
  let modify_outputs index txout =
    if index = input_index then txout
    else
      {
	transaction_output_value = Int64.minus_one;
	output_script = "";
      }
  in
  let remaining_outputs, _ = Utils.split_list tx.transaction_outputs (input_index + 1) in
  let modified_outputs = List.mapi modify_outputs remaining_outputs in
  let modified_inputs = zero_input_sequence_numbers tx.transaction_inputs input_index in
  { tx with
    transaction_inputs = modified_inputs;
    transaction_outputs = modified_outputs;
  }
;;
let checksig_copy_tx_sighashanyonecanpay tx input_index =
  let input = List.nth tx.transaction_inputs input_index in
  { tx with
    transaction_inputs = [input];
  }
;;

let one_hash = (String.make 31 '\x00') ^ "\x01";;

let checksig_check_for_one_hash tx input_index subscript hash_type flags =
  (input_index > (List.length tx.transaction_inputs)) ||
    (* bug backwards compatibiliy *)
    ((hash_type = SigHashSingle) && input_index > (List.length tx.transaction_outputs))
;;

let checksig_hash_transaction tx input_index subscript hash_type_byte hash_type flags =
  let txcopy = checksig_copy_tx tx input_index (Bitstring.string_of_bitstring (Bitcoin_script_generator.bitstring_of_script subscript)) in

  (* done with basic steps *)
  let txcopy = match hash_type with
    | SigHashAll | SigHashAllZero -> txcopy
    | SigHashNone -> checksig_copy_tx_sighashnone txcopy input_index
    | SigHashSingle -> checksig_copy_tx_sighashsingle txcopy input_index
    | UnknownHashType _ -> raise Result_invalid
  in
  let txcopy = if List.mem SigHashAnyoneCanPay flags then
      checksig_copy_tx_sighashanyonecanpay txcopy input_index
    else
      txcopy
  in

  let tx_bitstring = Bitcoin_protocol_generator.bitstring_of_transaction txcopy in
  (* quick-n-dirty one byte to 4 byte little endian... *)
  let hash_type_string = (String.make 1 (Char.chr hash_type_byte)) ^ (String.make 3 '\x00') in
  let tx_string = Bitstring.string_of_bitstring tx_bitstring ^ hash_type_string in

  Bitcoin_crypto.hash256 tx_string
;;

let op_checksig stack (tx, input_index) script_after_codesep =
  let subscript_filter signature = function
    | CodeSeparator -> false
    | Data (_, s) when (compare s signature) = 0 -> false
    | _ -> true
  in

  let pubkey = pop stack in
  let complete_signature = pop stack in

  let subscript = List.filter (subscript_filter complete_signature) script_after_codesep in

  let hash_type_byte = data_item_byte (-1) complete_signature in
  let hash_type, flags = hash_type_and_flags_of_int hash_type_byte in
  let signature = String.sub complete_signature 0 ((String.length complete_signature) - 1) in

  let hash = if checksig_check_for_one_hash tx input_index subscript hash_type flags then
      one_hash
    else
      checksig_hash_transaction tx input_index subscript hash_type_byte hash_type flags
  in

  let verification_result = Bitcoin_crypto_ecdsa.verify_der_signature pubkey hash signature in

  push (data_item_of_bool verification_result) stack
;;
let op_checkmultisig stack =
  ()
;;

let execute_word stack altstack tx_data script_data = function
  | Data (opcode, data_item) -> push data_item stack
  | Nop (opcode) -> ()
  | If -> raise Not_implemented
  | NotIf -> raise Not_implemented
  | Else -> raise Not_implemented
  | EndIf -> raise Not_implemented
  | Verify -> op_verify stack
  | Return -> raise Result_invalid
(* Stack manipulation *)
  | ToAltStack -> push (pop stack) altstack
  | FromAltStack -> push (pop altstack) stack
  | IfDup -> if bool_of_data_item (top stack) then push (top stack) stack
  | Depth -> push (data_item_of_int64 (Int64.of_int (Stack.length stack))) stack
  | Drop -> ignore (pop stack)
  | Dup -> push (top stack) stack
  | Nip -> op_nip stack
  | Over -> op_over stack
  | Pick -> op_pick stack
  | Roll -> op_roll stack
  | Rot -> roll_n 3 stack
  | Swap -> roll_n 2 stack
  | Tuck -> op_tuck stack
  | TwoDrop -> ignore (pop_to_list 2 stack)
  | TwoDup -> dup_n 2 stack
  | ThreeDup -> dup_n 3 stack
  | TwoOver -> op_2over stack
  | TwoRot -> op_2rot stack
  | TwoSwap -> op_2swap stack
(* Splice *)
  | Cat -> raise Disabled_opcode
  | Substr -> raise Disabled_opcode
  | Left -> raise Disabled_opcode
  | Right -> raise Disabled_opcode
  | Size -> op_size stack
(* Bitwise logic *)
  | Invert -> raise Disabled_opcode
  | And -> raise Disabled_opcode
  | Or -> raise Disabled_opcode
  | Xor -> raise Disabled_opcode
  | Equal -> op_equal stack
  | EqualVerify -> op_equal stack; op_verify stack
(* Arithmetic *)
  | OneAdd -> push_int32 (Int32.add (pop_int32 stack) 1l) stack
  | OneSub -> push_int32 (Int32.sub (pop_int32 stack) 1l) stack
  | TwoMul -> raise Disabled_opcode
  | TwoDiv -> raise Disabled_opcode
  | Negate -> push_int32 (Int32.neg (pop_int32 stack)) stack
  | Abs -> push_int32 (Int32.abs (pop_int32 stack)) stack
  | Not -> push_bool_arithmetic (not (pop_bool_arithmetic stack)) stack
  | ZeroNotEqual -> push_bool_arithmetic (pop_bool_arithmetic stack) stack
  | Add -> push_int32 (Int32.add (pop_int32 stack) (pop_int32 stack)) stack
  | Sub -> op_sub stack
  | Mul -> raise Disabled_opcode
  | Div -> raise Disabled_opcode
  | Mod -> raise Disabled_opcode
  | LShift -> raise Disabled_opcode
  | RShift -> raise Disabled_opcode
  | BoolAnd -> push_bool_arithmetic ((pop_bool_arithmetic stack) && (pop_bool_arithmetic stack)) stack
  | BoolOr -> push_bool_arithmetic ((pop_bool_arithmetic stack) || (pop_bool_arithmetic stack)) stack
  | NumEqual -> push_bool_arithmetic ((pop_int32 stack) = (pop_int32 stack)) stack
  | NumEqualVerify -> push_bool_arithmetic ((pop_int32 stack) = (pop_int32 stack)) stack; op_verify stack
  | NumNotEqual -> push_bool_arithmetic ((pop_int32 stack) != (pop_int32 stack)) stack
  | LessThan -> push_bool_arithmetic ((pop_int32 stack) > (pop_int32 stack)) stack
  | GreaterThan -> push_bool_arithmetic ((pop_int32 stack) < (pop_int32 stack)) stack
  | LessThanOrEqual -> push_bool_arithmetic ((pop_int32 stack) >= (pop_int32 stack)) stack
  | GreaterThanOrEqual -> push_bool_arithmetic ((pop_int32 stack) <= (pop_int32 stack)) stack
  | Min -> op_min stack
  | Max -> op_max stack
  | Within -> op_within stack
(* Cryptography *)
  | RIPEMD160 -> hash Bitcoin_crypto.ripemd160 stack
  | SHA1 -> hash Bitcoin_crypto.sha1 stack
  | SHA256 -> hash Bitcoin_crypto.sha256 stack
  | Hash160 -> hash Bitcoin_crypto.hash160 stack
  | Hash256 -> hash Bitcoin_crypto.hash256 stack
  | CodeSeparator -> ()
  | CheckSig -> op_checksig stack tx_data script_data
  | CheckSigVerify -> op_checksig stack tx_data script_data; op_verify stack
  | CheckMultiSig -> raise Not_implemented
  | CheckMultiSigVerify -> raise Not_implemented
(* Pseudo-words *)
  | PubKeyHash -> raise Result_invalid
  | PubKey -> raise Result_invalid
  | InvalidOpcode opcode -> raise Result_invalid
(* Reserved *)
  | Reserved -> raise Result_invalid
  | Ver -> raise Result_invalid
  | VerIf -> raise Result_invalid
  | VerNotIf -> raise Result_invalid
  | Reserved1 -> raise Result_invalid
  | Reserved2 -> raise Result_invalid
;;

let dump_stack stack =
  let index = ref 0 in
  let print_data_item_with_index item =
    Printf.printf "\t%d:\t%s\n" !index (Bitcoin_script_pp.pp_string_of_data_item item);
    index := !index + 1
  in
  Stack.iter print_data_item_with_index stack
;;

let execute_script script tx_data =
  let rec execute_script_ stack altstack tx_data script_after_codesep = function
    | [] -> (stack, altstack)
    | CodeSeparator :: ws ->
      execute_script_ stack altstack tx_data ws ws
    | word :: ws ->
      execute_word stack altstack tx_data script_after_codesep word;
      execute_script_ stack altstack tx_data (script_after_codesep @ (word :: ws)) ws
  in
  try
    let stack, altstack = execute_script_ (create_stack ()) (create_stack ()) tx_data [] script in
    Result (pop stack)
  with
  | Disabled_opcode -> Invalid
  | Result_invalid -> Invalid
  | Stack.Empty -> Invalid
;;
    
