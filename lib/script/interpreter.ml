open! Core.Std
open Bitcaml_utils.Std
open Bitcoin_crypto.Std
open Bitcoin_protocol.Std

let max_arithmetic_data_item_size = 4;;

module Hash_type = struct
  module Flag = struct
    module T = struct
      type t =
        | SigHashAnyoneCanPay
      [@@deriving compare, enumerate, sexp]
    end
    include T

    include Comparable.Make(T)

    let to_bitmask = function
      | SigHashAnyoneCanPay -> 0x80
    ;;

    let of_int i =
      List.fold all ~init:Set.empty ~f:(fun set t ->
        match Int.(>) (i land (to_bitmask t)) 0 with
        | false -> set
        | true  -> Set.add set t)
    ;;
  end

  type t =
    | SigHashAllZero (* bug backwards compatibility *)
    | SigHashAll
    | SigHashNone
    | SigHashSingle
    | Other of int
  [@@deriving compare, sexp]

  let of_int i =
    match (i land 0x0f) with
    | 0x00 -> SigHashAllZero
    | 0x01 -> SigHashAll
    | 0x02 -> SigHashNone
    | 0x03 -> SigHashSingle
    | _    -> Other i
  ;;

  let of_int_with_flags i =
    (of_int i, Flag.of_int i)
  ;;
end

let arithmetic_int64_of_data_item item =
  let length = Data_item.length item in
  if Int.(<=) length max_arithmetic_data_item_size then
    Data_item.to_int64 item
  else
    failwithf "arithmetic data item is longer than %d bytes" length ()
;;

module Stack = struct
  include Stack

  let pop = pop_exn
  let top = top_exn

  let pop_list t n =
    let rec pop' acc = function
      | 0 -> acc
      | n ->
        pop' (pop t :: acc) (pred n)
    in
    pop' [] n
  ;;

  let push_list t ls =
    List.iter ls ~f:(push t)
  ;;

  let pop_nth t n =
    let popped_items = pop_list t n in
    let item = pop t in
    push_list t popped_items;
    item
  ;;

  let top_nth t n =
    let popped_items = pop_list t n in
    let item = top t in
    push_list t popped_items;
    item
  ;;

  let push_int64 t i =
    Data_item.of_int64 i
    |> push t
  ;;

  let pop_int64 t =
    pop t
    |> arithmetic_int64_of_data_item
  ;;

  let push_bool_arithmetic t b =
    Data_item.of_bool b
    |> push t
  ;;

  let pop_bool_arithmetic t =
    pop_int64 t
    |> function
    | 0L -> false
    | _  -> true
  ;;
end

let roll_nth stack n =
  let item = Stack.pop_nth stack n in
  Stack.push stack item
;;

let dup_list stack n =
  let items = Stack.pop_list stack n in
  Stack.push_list stack items;
  Stack.push_list stack items
;;

let op_nip stack =
  let first = Stack.pop stack in
  ignore (Stack.pop stack);
  Stack.push stack first
;;

let op_over stack =
  let first = Stack.pop stack in
  let second = Stack.top stack in
  Stack.push stack first;
  Stack.push stack second
;;

let op_pick stack =
  let n =
    Data_item.to_int64 (Stack.pop stack)
    |> Int64.to_int_exn
  in
  let item = Stack.top_nth stack n in
  Stack.push stack item
;;

let op_roll stack =
  Stack.pop stack
  |> Data_item.to_int64
  |> Int64.to_int_exn
  |> roll_nth stack
;;

let op_tuck stack =
  let first  = Stack.pop stack in
  let second = Stack.pop stack in
  Stack.push_list stack [first; second; first]
;;

let op_2over stack =
  let first  = Stack.pop_list stack 2 in
  let second = Stack.pop_list stack 2 in
  Stack.push_list stack second;
  Stack.push_list stack first;
  Stack.push_list stack second
;;

let op_2rot stack =
  let first  = Stack.pop_list stack 2 in
  let second = Stack.pop_list stack 2 in
  let third  = Stack.pop_list stack 2 in
  Stack.push_list stack second;
  Stack.push_list stack first;
  Stack.push_list stack third
;;

let op_2swap stack =
  let first  = Stack.pop_list stack 2 in
  let second = Stack.pop_list stack 2 in
  Stack.push_list stack first;
  Stack.push_list stack second
;;

let op_size stack =
  Stack.top stack
  |> Data_item.length
  |> Int64.of_int
  |> Data_item.of_int64
  |> Stack.push stack
;;

let op_verify stack =
  Stack.pop stack
  |> Data_item.to_bool
  |> function
  | true  -> ()
  | false -> failwith "OP_VERIFY failed"
;;

let op_equal stack =
  Data_item.equal (Stack.pop stack) (Stack.pop stack)
  |> Data_item.of_bool
  |> Stack.push stack
;;

let op_not stack =
  Stack.pop_bool_arithmetic stack
  |> not
  |> Stack.push_bool_arithmetic stack
;;

let op_0notequal stack =
  Stack.pop_bool_arithmetic stack
  |> Stack.push_bool_arithmetic stack
;;

let minmax ~f stack =
  let b = Stack.pop_int64 stack in
  let a = Stack.pop_int64 stack in
  let c =
    if f a b then
      a
    else
      b
  in
  Stack.push_int64 stack c

let op_min = minmax ~f:(<)

let op_max = minmax ~f:(>)

let op_within stack =
  let max = Stack.pop_int64 stack in
  let min = Stack.pop_int64 stack in
  let x = Stack.pop_int64 stack in
  Stack.push_bool_arithmetic stack (min <= x && x < max)
;;

let binary_arithmetic_op_int64_result ~f stack =
  let op2 = Stack.pop_int64 stack in
  let op1 = Stack.pop_int64 stack in
  let result = f op1 op2 in
  Stack.push_int64 stack result
;;

let binary_arithmetic_op_boolean_result ~f stack =
  let op2 = Stack.pop_int64 stack in
  let op1 = Stack.pop_int64 stack in
  let result = f op1 op2 in
  Stack.push_bool_arithmetic stack result
;;

let binary_boolean_op_boolean_result ~f stack =
  let op2 = Stack.pop_bool_arithmetic stack in
  let op1 = Stack.pop_bool_arithmetic stack in
  let result = f op1 op2 in
  Stack.push_bool_arithmetic stack result
;;

let op_add = binary_arithmetic_op_int64_result ~f:Int64.(+)

let op_sub = binary_arithmetic_op_int64_result ~f:Int64.(-)

let transform ~f stack =
  Stack.pop stack
  |> f
  |> Hash_string.to_bytes
  |> Stack.push stack
;;

module Signature = struct
  let one_hash =
    (String.make 31 '\x00') ^ "\x01"
    |> Hash_string.of_bytes
  ;;

  let copy_tx ~transaction ~input_index ~subscript =
    let inputs =
      Map.change
        (Transaction.inputs transaction)
        input_index
        ~f:(Option.map ~f:(Transaction.Input.update ~signature_script:subscript))
    in
    Transaction.update
      ~inputs
      transaction
  ;;

  let zero_input_sequence_numbers ~inputs ~input_index =
    Map.change
      inputs
      input_index
      ~f:(Option.map ~f:(Transaction.Input.update ~sequence_number:0x0000l))
  ;;

  let copy_tx_sighashnone ~transaction ~input_index =
    let inputs =
      zero_input_sequence_numbers
        ~inputs:(Transaction.inputs transaction)
        ~input_index
    in
    Transaction.update
      ~inputs
      ~outputs:Int.Map.empty
      transaction
  ;;

  let copy_tx_sighashsingle ~transaction ~input_index =
    let outputs =
      Transaction.outputs transaction
      |> Map.to_sequence
           ~order:`Increasing_key
           ~keys_less_or_equal_to:(pred input_index)
      |> Sequence.map ~f:(fun (index, output) ->
        let output =
          Transaction.Output.update
            ~value:Int64.minus_one
            output
        in
        (index, output))
      |> Sequence.fold ~init:Int.Map.empty ~f:(fun map (key, data) ->
        Map.add map ~key ~data)
    in
    let inputs =
      zero_input_sequence_numbers
        ~inputs:(Transaction.inputs transaction)
        ~input_index
    in
    Transaction.update
      ~inputs
      ~outputs
      transaction
  ;;

  let copy_tx_sighashanyonecanpay ~transaction ~input_index =
    let input =
      Map.find (Transaction.inputs transaction) input_index
      |> Option.value_exn
           ~message:"missing transaction input for sighash anyonecanpay"
           ~here:[%here]
    in
    Transaction.update
      ~inputs:(Int.Map.singleton input_index input)
      transaction
  ;;

  let hash_transaction ~transaction ~input_index ~subscript ~hash_type_byte ~hash_type ~flags =
    let transaction =
      copy_tx
        ~transaction
        ~input_index
        ~subscript:(Bitstring.string_of_bitstring (Script.to_bitstring subscript))
    in
    let transaction =
      match (hash_type : Hash_type.t) with
      | SigHashAll
      | SigHashAllZero -> transaction
      | SigHashNone    -> copy_tx_sighashnone   ~transaction ~input_index
      | SigHashSingle  -> copy_tx_sighashsingle ~transaction ~input_index
      | Other i        -> failwithf "invalid hash type: %d" i ()
    in
    let transaction =
      if Set.mem flags Hash_type.Flag.SigHashAnyoneCanPay then
        copy_tx_sighashanyonecanpay ~transaction ~input_index
      else
        transaction
    in
    let transaction =
      Transaction.to_bitstring transaction
      |> Bitstring.string_of_bitstring
    in
    (* quick-n-dirty one byte to 4 byte little endian... *)
    let hash_type_string =
      (String.of_char (Char.of_int_exn hash_type_byte)) ^ (String.make 3 '\x00')
    in
    Hash_string.hash256 (transaction ^ hash_type_string)
  ;;

  let check_for_one_hash ~transaction ~input_index ~hash_type =
    (* bug backwards compatibiliy *)
    let sighash_single_bug =
      match (hash_type : Hash_type.t) with
      | SigHashSingle -> input_index > (Map.length (Transaction.outputs transaction))
      | _             -> false
    in
    sighash_single_bug || (input_index > (Map.length (Transaction.inputs transaction)))
  ;;

  let check ~public_key ~complete_signature ~transaction ~input_index ~subscript =
    let hash_type_byte = Data_item.nget_exn complete_signature (-1) in
    let hash_type, flags = Hash_type.of_int_with_flags hash_type_byte in
    let der_signature =
      String.sub
        complete_signature
        ~pos:0
        ~len:(String.length complete_signature - 1)
    in
    let hash =
      if check_for_one_hash ~transaction ~input_index ~hash_type then
        one_hash
      else
        hash_transaction
          ~transaction
          ~input_index
          ~subscript
          ~hash_type_byte
          ~hash_type
          ~flags
    in
    Ecdsa.verify_der_signature
      ~public_key
      ~hash
      ~der_signature
  ;;
end

let op_checksig ~stack ~transaction ~input_index ~script_after_codesep =
  let public_key         = Stack.pop stack in
  let complete_signature = Stack.pop stack in
  let subscript =
    List.filter script_after_codesep ~f:(function
      | Word.CodeSeparator      -> false
      | Data {data; opcode = _} -> String.(<>) data complete_signature
      | _                       -> true)
  in
  Signature.check
    ~public_key
    ~complete_signature
    ~transaction
    ~input_index
    ~subscript
  |> Data_item.of_bool
  |> Stack.push stack
;;

let op_checkmultisig ~stack ~transaction ~input_index ~script_after_codesep =
  let rec check_signatures_against_public_keys ~signatures ~public_keys ~subscript =
    match signatures, public_keys with
    | [], _ -> true
    | _, [] -> false
    | signature :: signatures, public_key :: public_keys ->
      if Signature.check ~public_key ~complete_signature:signature ~transaction ~input_index ~subscript then
	check_signatures_against_public_keys
          ~signatures
          ~public_keys
          ~subscript
      else
        check_signatures_against_public_keys
          ~signatures:(signature :: signatures)
          ~public_keys
          ~subscript
  in
  match Stack.pop_int64 stack with
  | i when (i > 20L) || (i < 0L) ->
    Stack.push stack (Data_item.of_bool false)
  | public_key_count ->
    let public_keys = Stack.pop_list stack (Int64.to_int_exn public_key_count) in
    match Stack.pop_int64 stack with
    | i when (i < 0L) || (i > public_key_count) ->
      Stack.push stack (Data_item.of_bool false)
    | signature_count ->
      let signatures = Stack.pop_list stack (Int64.to_int_exn signature_count) in
      ignore(Stack.pop stack);
      let subscript =
        List.filter script_after_codesep ~f:(function
          | Word.CodeSeparator -> false
          | Data {data; opcode = _} ->
            List.mem ~equal:String.equal signatures data
            |> not
          | _ -> true)
      in
      check_signatures_against_public_keys ~signatures ~public_keys ~subscript
      |> Data_item.of_bool
      |> Stack.push stack
;;

let execute_word ~stack ~altstack ~transaction ~input_index ~script_after_codesep : Word.t -> unit = function
  | Data {data; opcode = _} -> Stack.push stack data
  | Nop _                   -> ()
  | If                      -> failwith "opcode not implemented"
  | NotIf                   -> failwith "opcode not implemented"
  | Else                    -> failwith "opcode not implemented"
  | EndIf                   -> failwith "opcode not implemented"
  | Verify                  -> op_verify stack
  | Return                  -> failwith "result invalid"
  | ToAltStack              -> Stack.push altstack (Stack.pop stack)
  | FromAltStack            -> Stack.push stack    (Stack.pop altstack)
  | IfDup                   -> if Data_item.to_bool (Stack.top stack) then Stack.push stack (Stack.top stack)
  | Depth                   -> Stack.push stack (Data_item.of_int64 (Int64.of_int (Stack.length stack)))
  | Drop                    -> ignore (Stack.pop stack)
  | Dup                     -> Stack.push stack (Stack.top stack)
  | Nip                     -> op_nip stack
  | Over                    -> op_over stack
  | Pick                    -> op_pick stack
  | Roll                    -> op_roll stack
  | Rot                     -> roll_nth stack 2
  | Swap                    -> roll_nth stack 1
  | Tuck                    -> op_tuck stack
  | TwoDrop                 -> ignore (Stack.pop_list stack 2)
  | TwoDup                  -> dup_list stack 2
  | ThreeDup                -> dup_list stack 3
  | TwoOver                 -> op_2over stack
  | TwoRot                  -> op_2rot stack
  | TwoSwap                 -> op_2swap stack
  | Cat                     -> failwith "disabled opcode"
  | Substr                  -> failwith "disabled opcode"
  | Left                    -> failwith "disabled opcode"
  | Right                   -> failwith "disabled opcode"
  | Size                    -> op_size stack
  | Invert                  -> failwith "disabled opcode"
  | And                     -> failwith "disabled opcode"
  | Or                      -> failwith "disabled opcode"
  | Xor                     -> failwith "disabled opcode"
  | Equal                   -> op_equal stack
  | EqualVerify             -> op_equal stack; op_verify stack
  | OneAdd                  -> Stack.push_int64 stack (Int64.(+) (Stack.pop_int64 stack) 1L)
  | OneSub                  -> Stack.push_int64 stack (Int64.(-) (Stack.pop_int64 stack) 1L)
  | TwoMul                  -> failwith "disabled opcode"
  | TwoDiv                  -> failwith "disabled opcode"
  | Negate                  -> Stack.push_int64 stack (Int64.neg (Stack.pop_int64 stack))
  | Abs                     -> Stack.push_int64 stack (Int64.abs (Stack.pop_int64 stack))
  | Not                     -> op_not stack
  | ZeroNotEqual            -> op_0notequal stack
  | Add                     -> op_add stack
  | Sub                     -> op_sub stack
  | Mul                     -> failwith "disabled opcode"
  | Div                     -> failwith "disabled opcode"
  | Mod                     -> failwith "disabled opcode"
  | LShift                  -> failwith "disabled opcode"
  | RShift                  -> failwith "disabled opcode"
  | BoolAnd                 -> binary_boolean_op_boolean_result ~f:( && ) stack
  | BoolOr                  -> binary_boolean_op_boolean_result ~f:( || ) stack
  | NumEqual                -> binary_arithmetic_op_boolean_result ~f:( = ) stack
  | NumEqualVerify          -> binary_arithmetic_op_boolean_result ~f:( = ) stack; op_verify stack
  | NumNotEqual             -> binary_arithmetic_op_boolean_result ~f:( <> ) stack
  | LessThan                -> binary_arithmetic_op_boolean_result ~f:( < ) stack
  | GreaterThan             -> binary_arithmetic_op_boolean_result ~f:( > ) stack
  | LessThanOrEqual         -> binary_arithmetic_op_boolean_result ~f:( <= ) stack
  | GreaterThanOrEqual      -> binary_arithmetic_op_boolean_result ~f:( >= ) stack
  | Min                     -> op_min stack
  | Max                     -> op_max stack
  | Within                  -> op_within stack
  | RIPEMD160               -> transform ~f:Hash_string.ripemd160 stack
  | SHA1                    -> transform ~f:Hash_string.sha1      stack
  | SHA256                  -> transform ~f:Hash_string.sha256    stack
  | Hash160                 -> transform ~f:Hash_string.hash160   stack
  | Hash256                 -> transform ~f:Hash_string.hash256   stack
  | CodeSeparator           -> ()
  | CheckSig                -> op_checksig ~stack ~transaction ~input_index ~script_after_codesep
  | CheckSigVerify          -> op_checksig ~stack ~transaction ~input_index ~script_after_codesep; op_verify stack
  | CheckMultiSig           -> op_checkmultisig ~stack ~transaction ~input_index ~script_after_codesep
  | CheckMultiSigVerify     -> op_checkmultisig ~stack ~transaction ~input_index ~script_after_codesep; op_verify stack
  | PubKeyHash              -> failwith "result invalid"
  | PubKey                  -> failwith "result invalid"
  | InvalidOpcode _         -> failwith "result invalid"
  | Reserved                -> failwith "result invalid"
  | Ver                     -> failwith "result invalid"
  | VerIf                   -> failwith "result invalid"
  | VerNotIf                -> failwith "result invalid"
  | Reserved1               -> failwith "result invalid"
  | Reserved2               -> failwith "result invalid"
;;

let dump_stack stack =
  Stack.fold stack ~init:0 ~f:(fun i item ->
    printf !"\t% 5d:\t%{Data_item#hum}\n" i item;
    succ i)
  |> fun (_ : int) -> ()
;;

let dump_ifstack ~not_taken_if_level ifstack  =
  Printf.printf "[SCRIPT] NOT_TAKEN_IF_LEVEL: %d\n" not_taken_if_level;
  Stack.fold ifstack ~init:0 ~f:(fun i item ->
    printf "\t% 5d:\t%b\n" i item;
    succ i)
  |> fun (_ : int) -> ()
;;

let execute_script ?(debug=false) ~transaction ~input_index script =
  let ifstack  : bool        Stack.t = Stack.create () in
  let stack    : Data_item.t Stack.t = Stack.create () in
  let altstack : Data_item.t Stack.t = Stack.create () in
  let branch_is_executing ~not_taken_if_level =
    (Int.equal not_taken_if_level 0)
    && ((Stack.is_empty ifstack) || (Stack.top_exn ifstack))
  in
  let rec execute_script_ ~not_taken_if_level ~script_after_codesep = function
    | [] -> ()
    | Word.CodeSeparator :: ws ->
      execute_script_
        ~not_taken_if_level
        ~script_after_codesep:ws
        ws
    | If :: ws ->
      if branch_is_executing ~not_taken_if_level then begin
	let value = Data_item.to_bool (Stack.pop stack) in
        Stack.push ifstack value;
        if debug then begin
          printf "[SCRIPT] executing word: If (value: %b)\n" value;
          dump_ifstack ~not_taken_if_level ifstack
        end;
        execute_script_
          ~not_taken_if_level
          ~script_after_codesep
          ws
      end else
	execute_script_
          ~not_taken_if_level:(succ not_taken_if_level)
          ~script_after_codesep
          ws
    | NotIf :: ws ->
      if branch_is_executing ~not_taken_if_level then begin
	let value = Data_item.to_bool (Stack.pop stack) in
        Stack.push ifstack (not value);
        if debug then begin
	  printf "[SCRIPT] executing word: NotIf (value: %b)\n" value;
          dump_ifstack ~not_taken_if_level ifstack
        end;
        execute_script_
          ~not_taken_if_level
          ~script_after_codesep
          ws
      end else
	execute_script_
          ~not_taken_if_level:(succ not_taken_if_level)
          ~script_after_codesep
          ws
    | Else :: ws ->
      if Int.equal not_taken_if_level 0 then begin
	Stack.push ifstack (not (Stack.pop ifstack));
        if debug then begin
          printf "[SCRIPT] executing word: Else\n";
          dump_ifstack ~not_taken_if_level ifstack
        end;
      end;
      execute_script_
        ~not_taken_if_level
        ~script_after_codesep
        ws
    | EndIf :: ws ->
      if branch_is_executing ~not_taken_if_level then begin
	ignore (Stack.pop ifstack);
        if debug then begin
          printf "[SCRIPT] executing word: EndIf\n";
          dump_ifstack ~not_taken_if_level ifstack
        end;
        execute_script_
          ~not_taken_if_level
          ~script_after_codesep
          ws
      end else
	execute_script_
          ~not_taken_if_level:(min 0 (pred not_taken_if_level))
          ~script_after_codesep
          ws
    | word :: ws ->
      if branch_is_executing ~not_taken_if_level then begin
        if debug then begin
	  printf !"[SCRIPT] executing word: %{Word#hum}\n" word
        end;
	execute_word
          ~stack
          ~altstack
          ~transaction
          ~input_index
          ~script_after_codesep
          word;
          if debug then begin
	    if (Stack.length stack) < 5 then dump_stack stack;
            printf "///////////////////////////////"
          end;
      end;
      execute_script_
        ~not_taken_if_level
        ~script_after_codesep
        ws
  in
  execute_script_
    ~not_taken_if_level:0
    ~script_after_codesep:script
    script;
  Stack.pop stack
;;
