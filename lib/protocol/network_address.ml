open! Core.Std
open Bitcaml_utils.Std

type t =
  { services : Service.Set.t
  ; host     : Bitstring.t
  ; port     : int
  } [@@deriving bin_io, compare, fields, sexp]

let create = Fields.create

let of_bitstring = function%bitstring
  | {| services :  8*8 : littleendian
     ; host     : 16*8 : bitstring
     ; port     :  2*8 : bigendian
     ; rest     :   -1 : bitstring
    |} ->
    let t =
      Fields.create
        ~services:(Service.of_int64 services)
        ~host
        ~port
    in
    t, rest
  | {| _ |} -> failwith "invalid network address"
;;

let to_bitstring t =
  let services = Service.to_int64 t.services in
  [%bitstring
    {| services :  8*8 : littleendian
     ; t.host   : 16*8 : bitstring
     ; t.port   :  2*8 : bigendian
    |}]
;;
