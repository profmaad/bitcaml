open! Core.Std

let map_string ~f s =
  String.to_list s
  |> List.map ~f
;;

let print_hex_string ?(line_length=0) ?(indentation=0) s =
  let indent = String.make indentation '\t' in
  let hex_iterator index c =
    if (index > 0) && (line_length > 0) && ((index mod line_length) = 0) then begin
      Out_channel.newline stdout;
      print_string indent
    end;
    Printf.printf "%02x " (Char.to_int c);
  in
  print_string indent;
  String.to_list s
  |> List.iteri ~f:hex_iterator
;;

let hex_string_of_string ?(sep="") s =
  map_string s ~f:(fun c -> sprintf "%02x" (Char.to_int c))
  |> String.concat ~sep
;;

let hex_encode s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s
let hex_decode s = Cryptokit.transform_string (Cryptokit.Hexa.decode ()) s

let string_of_zeroterminated_string s =
  String.rstrip ~drop:(Char.equal '\x00') s
;;

let zeropad_string ~length s =
  s ^ (String.make (length - (String.length s)) '\x00')
;;
