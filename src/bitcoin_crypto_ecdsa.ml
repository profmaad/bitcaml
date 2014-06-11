open Bitstring;;

let ensure_string_length s length =
  if (String.length s) > length then
    String.sub s 0 length
  else
    String.make (length - (String.length s)) '\x00' ^ s
;;
let parse_der_signature bits =
  bitmatch bits with
  | { "\x30" : 1*8 : string;
      overall_length : 1*8 : littleendian;
      "\x02" : 1*8 : string;
      r_length : 1*8 : littleendian;
      r : r_length * 8 : string;
      "\x02" : 1*8 : string;
      s_length : 1*8 : littleendian;
      s : s_length * 8 : string
    } ->
    Some (r, s)
  | { _ } -> None
;;

let extract_public_key s =
  if s.[0] = '\x04' then (
    let public_key = String.sub s 1 ((String.length s) - 1) in
    ensure_string_length public_key 64
  ) else (
    Microecc.decompress s
  )
;;

let verify_der_signature public_key hash der_signature =
  let public_key = extract_public_key public_key in
  match parse_der_signature (bitstring_of_string der_signature) with
  | Some (r, s) ->
    Microecc.verify public_key hash ((ensure_string_length r 32) ^ (ensure_string_length s 32))
  | None -> false
;;
