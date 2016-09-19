open! Core.Std

type t =
  | MainNetwork
  | TestNet
  | TestNet3
  | Other of int32
[@@deriving compare, sexp]

let of_int32 = function
  | 0xD9B4BEF9l -> MainNetwork
  | 0xDAB5BFFAl -> TestNet
  | 0x0709110Bl -> TestNet3
  | i           -> Other i
;;

let to_int32 = function
  | MainNetwork -> 0xD9B4BEF9l
  | TestNet     -> 0xDAB5BFFAl
  | TestNet3    -> 0x0709110Bl
  | Other i     -> i
;;
