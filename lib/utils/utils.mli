open! Core.Std

val print_hex_string
  :  ?line_length:int
  -> ?indentation:int
  -> string
  -> unit

val hex_string_of_string
  :  ?sep:string
  -> string
  -> string

val hex_encode : string -> string
val hex_decode : string -> string

val string_of_zeroterminated_string : string -> string

val zeropad_string : length:int -> string -> string
