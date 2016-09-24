open! Core.Std

type t = Word.t list [@@deriving bin_io, compare, sexp]

include Bitcaml_utils.Std.Bitstringable with type t := t

val to_string_hum : t -> string

val sigop_count : t -> int
