let map_string f s =
  let rec map_string_ f s position length =
    if position >= length then []
    else
      (f s.[position]) :: map_string_ f s (position + 1) length
  in
  map_string_ f s 0 (String.length s)
;;
let reverse_string s =
  let rec reverse_string_acc s acc index length =
    if index >= length
    then acc
    else reverse_string_acc s ((String.make 1 s.[index]) ^ acc) (index+1) length
  in
  reverse_string_acc s "" 0 (String.length s)
;;

let print_hex_string s line_length =
  let hex_iterator index c =
    if (index > 0) && (line_length > 0) && ((index mod line_length) = 0) then print_newline ();
    Printf.printf "%02x " (int_of_char c);
  in
  String.iteri hex_iterator s
;;
let print_indented_hex_string s line_length indent_level =
  let hex_iterator index c =
    if (index > 0) && (line_length > 0) && ((index mod line_length) = 0) then print_string ("\n" ^ (String.make indent_level '\t'));
    Printf.printf "%02x " (int_of_char c);
  in
  print_string (String.make indent_level '\t');
  String.iteri hex_iterator s
;;

let hex_string_of_string s =
  let hex_mapper c = Printf.sprintf "%02x " (int_of_char c) in
  String.concat "" (map_string hex_mapper s)
;;
let hex_string_of_hash_string s =
  let hex_mapper c = Printf.sprintf "%02x" (int_of_char c) in
  String.concat "" (map_string hex_mapper (reverse_string s))
;;
let hex_string_of_hash_string_noreverse s =
  let hex_mapper c = Printf.sprintf "%02x" (int_of_char c) in
  String.concat "" (map_string hex_mapper s)
;;
let zero_hash = String.make 32 '\x00';;

let reverse_hash_string s =
  let rec blit_reverse_hash_string src dst length byte_index =
    if byte_index*2 < length then (
      String.blit src (byte_index*2) dst (length - ((byte_index + 1) * 2)) 2;
      blit_reverse_hash_string src dst length (byte_index + 1)
    )
    else dst
  in
  let length = String.length s in
  if (length mod 2) != 0 then raise (Invalid_argument "Input length not a multiple of 2")
  else
    let dst = String.make length '\x00' in
    blit_reverse_hash_string s dst length 0
;;

let bytestring_of_int64 i bytesize =
  let rec bytestring_of_int_ i acc byte_index =
    let shift_distance = 8 * byte_index in
    let mask = Int64.shift_left 0xffL shift_distance in
    let masked_int = Int64.logand i mask in
    let shifted_int = Int64.shift_right_logical masked_int shift_distance in
    let byte_char = Char.chr (Int64.to_int shifted_int) in
    let new_acc = acc ^ (String.make 1 byte_char) in
    match byte_index with
    | 0 -> new_acc
    | byte_index -> bytestring_of_int_ i new_acc (byte_index-1)
  in
  bytestring_of_int_ i "" (bytesize-1)
;;
let le_bytestring_of_int64 i bytesize =
  reverse_string (bytestring_of_int64 i bytesize)
;;

let unix_tm_of_now () = Unix.localtime (Unix.time ());;
let unix_tm_of_int32 timestamp = 
  Unix.localtime (Int32.to_float timestamp)
;;
let unix_tm_of_int64 timestamp = 
  Unix.localtime (Int64.to_float timestamp)
;;
let int32_of_unix_tm unix_tm = 
  let timestamp, _ = Unix.mktime unix_tm in
  Int32.of_float timestamp
;;
let int64_of_unix_tm unix_tm = 
  let timestamp, _ = Unix.mktime unix_tm in
  Int64.of_float timestamp
;;
let string_of_unix_tm time =
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (time.Unix.tm_year + 1900) (time.Unix.tm_mon + 1) time.Unix.tm_mday time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec
;;
let string_of_timestamp timestamp =
  string_of_unix_tm (unix_tm_of_int64 timestamp)
;;

let string_from_zeroterminated_string zts =
  let string_length =
    try
      String.index zts '\x00'
    with Not_found -> 12
  in
  String.sub zts 0 string_length
;;
let zeropad_string_to_length s length =
  s ^ (String.make (length - (String.length s)) '\x00')
;;

let int_of_bool = function
  | false -> 0
  | true -> 1
;;

let remove_nth list position =
  let rec remove_nth_ list position acc =
    match list, position with
    | [], _ -> List.rev acc
    | x :: xs, 0 -> (List.rev acc) @ xs
    | x :: xs, p -> remove_nth_ xs (position - 1) (x :: acc)
  in
  remove_nth_ list position []
;;

let split_list list length =
  let rec split_list_ list length acc =
    match list, length with
    | [], _ -> (acc, [])
    | xs, 0 -> (acc, xs)
    | x :: xs, l -> split_list_ xs (l - 1) (x :: acc)
  in
  let first, second = split_list_ list length [] in
  (List.rev first, second)
;;
