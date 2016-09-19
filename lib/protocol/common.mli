open! Core.Std

module Varint : sig
  type t = int64 [@@deriving compare, sexp]

  include Bitstringable.S with type t := t
end

module Varstring : sig
  type t = string [@@deriving compare, sexp]

  val to_bitstring : t -> Bitstring.t

  val of_bitstring
    :  name:string
    -> Bitstring.t
    -> (t * Bitstring.t)
end

module Varlist : sig
  val fixed_length_string_of_bitstring
    :  name:string
    -> length:int
    -> Bitstring.t
    -> (string * Bitstring.t)

  val fixed_length_string_to_bitstring
    :  length:int
    -> string
    -> Bitstring.t

  val of_bitstring
    :  element_of_bitstring:(Bitstring.t -> ('a * Bitstring.t))
    -> Bitstring.t
    -> ('a list * Bitstring.t)

  val to_bitstring
    :  bitstring_of_element:('a -> Bitstring.t)
    -> 'a list
    -> Bitstring.t
end
