open! Core.Std

module type Validatable = sig
  type t [@@deriving sexp_of]

  val predicates : (string, t -> bool) List.Assoc.t
end

module type S = sig
  type t

  val validate : t -> unit Or_error.t
end

module Make (V : Validatable) : (S with type t := V.t) = struct
  let validate t =
    List.find_map V.predicates ~f:(fun (name, predicate) ->
      match predicate t with
      | true  -> None
      | false -> Some name)
    |> Option.value_map ~default:Result.ok_unit ~f:(fun failed_predicate ->
      Or_error.error_s [%message
        "validation failed"
          (failed_predicate : string)
          (t : V.t)
      ])
  ;;
end
