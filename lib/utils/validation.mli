open! Core.Std

module type Validatable = sig
  type t [@@deriving sexp_of]

  val predicates : (string, t -> bool) List.Assoc.t
end

module type S = sig
  type t

  val validate : t -> unit Or_error.t
end

module Make (V : Validatable) : (S with type t := V.t)
