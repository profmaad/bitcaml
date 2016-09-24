open! Core.Std

val print_hex_string
  :  ?line_length:int
  -> ?indentation:int
  -> string
  -> unit

val string_of_zeroterminated_string : string -> string

val zeropad_string : length:int -> string -> string
