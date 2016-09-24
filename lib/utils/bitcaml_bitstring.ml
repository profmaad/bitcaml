open Core.Std

include Bitstring

let to_string = string_of_bitstring
let of_string = bitstring_of_string

let t_of_sexp sexp =
  String.t_of_sexp sexp
  |> bitstring_of_string
;;

let sexp_of_t t =
  string_of_bitstring t
  |> String.sexp_of_t
;;

let bin_shape_t = bin_shape_string

let bin_size_t = Fn.compose bin_size_string to_string

let bin_write_t buf ~pos t =
  to_string t
  |> bin_write_string buf ~pos
;;

let bin_writer_t =
  { Bin_prot.Type_class
    . size = bin_size_t
  ; write = bin_write_t
  }
;;

let bin_read_t buf ~pos_ref =
  bin_read_string buf ~pos_ref
  |> of_string
;;

let __bin_read_t__ _buf ~pos_ref _vint =
  Bin_prot.Common.raise_variant_wrong_type "bitstring" !pos_ref
;;

let bin_reader_t =
  let vtag_read buf ~pos_ref = __bin_read_t__ buf ~pos_ref 0 in
  { Bin_prot.Type_class
    . read = bin_read_t
  ; vtag_read
  }
;;

let bin_t =
  { Bin_prot.Type_class
    . shape = bin_shape_t
  ; writer = bin_writer_t
  ; reader = bin_reader_t
  }
;;
