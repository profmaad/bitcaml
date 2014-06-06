open Bitstring;;
open Bitcoin_script;;

exception Malformed_script;;

let parse_int bytes bits =
  bitmatch bits with
  | { i : bytes * 8 : littleendian;
      rest : -1 : bitstring
    } ->
    (i, rest)
  | { _ } -> raise Malformed_script
;;

let parse_data opcode bits =
  match opcode with
  | 0x00 -> (Data (0x00, ""), bits)
  | 0x4f -> (Data (0x00, "\x81"), bits)
  | i when (i >= 0x51 && i <= 0x60) -> (Data (opcode, String.make 1 (Char.chr (i - 0x50))), bits)
  | i when (i >= 0x4c && i <= 0x4e) ->
    let bytes = match i with
      | 0x4c -> 1
      | 0x4d -> 2
      | 0x4e -> 4
      | _ -> raise Malformed_script (* this can't be reached, but pattern matching must be exhaustive *)
    in
    let data_length, bits = parse_int bytes bits in
    let data_length = (Int64.to_int data_length) * 8 in
    (Data (i, string_of_bitstring (takebits data_length bits)), dropbits data_length bits)
  | i when (i >= 0x01 && i <= 0x4b) ->
    let data_length = i * 8 in
    (Data (i, string_of_bitstring (takebits data_length bits)), dropbits data_length bits)
  | i -> raise Malformed_script
;;

let parse_opcode opcode bits =
  match word_of_opcode opcode with
  | Data (opcode, data) -> parse_data opcode bits
  | word -> (word, bits)
;;

let parse_script bits =
  let rec parse_script_acc acc bits = 
    bitmatch bits with
    | { opcode : 1*8 : littleendian;
	rest : -1 : bitstring
      } -> 
      let word, bits = parse_opcode opcode rest in
      parse_script_acc (word :: acc) bits
    | { _ } -> acc
  in
  List.rev (parse_script_acc [] bits)
;;
