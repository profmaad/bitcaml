open! Core.Std

type t [@@deriving compare, sexp]

include Bitstringable.S with type t := t

val create
  :  services:Service.Set.t
  -> host:Bitstring.t
  -> port:int
  -> t

val services : t -> Service.Set.t
val host     : t -> Bitstring.t
val port     : t -> int
