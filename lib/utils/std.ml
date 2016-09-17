open! Core.Std

module Utils = Utils

module Bitstring = struct
  include Bitstring

  let t_of_sexp sexp =
    String.t_of_sexp sexp
    |> bitstring_of_string
  ;;

  let sexp_of_t t =
    string_of_bitstring t
    |> String.sexp_of_t
  ;;
end
