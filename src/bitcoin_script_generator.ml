open Bitstring;;
open Bitcoin_script;;

let bitstring_of_data_item = bitstring_of_string;;

let bitstring_of_data_word opcode data_item =
  let length = data_item_length data_item in
  let payload = match opcode with
    | 0x00 | 0x4f -> empty_bitstring
    | i when (i >= 0x51 && i <= 0x60) -> empty_bitstring
    | i when (i >= 0x01 && i <= 0x4b) ->
      if length <> opcode then raise Malformed_script
      else bitstring_of_data_item data_item
    | 0x4c ->
      if length > 0xff then raise Malformed_script
      else
        let%bitstring bs =
          {| length                           : 1*8 : littleendian
           ; bitstring_of_data_item data_item :  -1 : bitstring
           |}
        in
        bs
    | 0x4d ->
      if length > 0xffff then raise Malformed_script
      else
	let%bitstring bs =
          {| length : 2*8 : littleendian;
	   bitstring_of_data_item data_item : -1 : bitstring
	   |}
        in
        bs
    | 0x4e ->
      if length > 0xffffffff then raise Malformed_script
      else
	let%bitstring bs =
          {| Int32.of_int length : 4*8 : littleendian;
	   bitstring_of_data_item data_item : -1 : bitstring
	   |}
        in
        bs
    | _ -> raise Malformed_script
  in
  let%bitstring bs =
    {| opcode : 1*8 : littleendian;
     payload : -1 : bitstring
     |}
  in
  bs
;;

let bitstring_of_word = function
  | Data (opcode, data_item) -> bitstring_of_data_word opcode data_item
  | word ->
     let%bitstring bs =
       {| opcode_of_word word : 1*8 : littleendian
        |}
     in
     bs
;;

let bitstring_of_script script =
  concat (List.map bitstring_of_word script)
;;
