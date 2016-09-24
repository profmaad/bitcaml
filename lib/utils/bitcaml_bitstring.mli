open! Core.Std

include module type of Bitstring

include Sexpable.S   with type t := t
include Binable.S    with type t := t
include Stringable.S with type t := t
