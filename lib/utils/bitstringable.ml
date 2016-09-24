module type S = sig
  type t

  val to_bitstring : t -> Bitstring.t
  val of_bitstring : Bitstring.t -> (t * Bitstring.t)
end
