open! Core.Std

type t = string [@@deriving bin_io, compare, sexp]

let length = String.length
let equal  = String.equal

let to_string = Fn.id
let of_string = Fn.id

let to_bitstring = Bitstring.bitstring_of_string
let of_bitstring bits = Bitstring.(string_of_bitstring bits, empty_bitstring)

let nget_exn t n =
  String.nget t n
  |> Char.to_int
;;

let to_int64 t =
  String.to_list_rev t
  |> List.map ~f:Char.to_int
  |> function
  | [] -> 0L
  | _ when (length t > 9) -> failwith "data item can't be converted to int64"
  | hd :: tl ->
    let is_negative = hd land 0x80 > 0 in
    let hd = hd land (lnot 0x80) in
    let i =
      List.fold (hd :: tl) ~init:Int64.zero ~f:(fun i byte ->
        Int64.of_int byte
        |> Int64.bit_or i
        |> fun i ->
        Int64.shift_left i 8)
    in
    if is_negative then
      Int64.neg i
    else
      i
;;

let of_int64 i =
  let rec bytes_of_int64 acc i =
    if i > 0L then
      let byte =
        Int64.bit_and 0xffL i
        |> Int64.to_int_exn
      in
      let i = Int64.shift_right_logical i 8 in
      bytes_of_int64 (byte :: acc) i
    else
      acc
  in
  match bytes_of_int64 [] (Int64.abs i) with
  | [] -> ""
  | msb :: tl ->
    let msb, tl =
      if msb land 0x80 > 0 then
        0x00, msb :: tl
      else
        msb, tl
    in
    let bytes =
      match Int64.is_negative i with
      | false -> msb :: tl
      | true  -> (msb lor 0x80) :: tl
    in
    List.rev bytes
    |> List.map ~f:Char.of_int_exn
    |> String.of_char_list
;;

let to_bool t =
  match to_int64 t with
  | 0L -> false
  | _  -> true
;;

let of_bool = function
  | false -> ""
  | true  -> "\x01"
;;

let to_string_hum t =
  match Or_error.try_with (fun () -> to_int64 t) with
  | Error _ -> t
  | Ok    i -> sprintf !"%s (%{Int64})" t i
;;
