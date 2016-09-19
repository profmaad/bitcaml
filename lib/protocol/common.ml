open! Core.Std

module Varint = struct
  type t = int64 [@@deriving compare, sexp]

  let of_bitstring bits =
    let parse_value bits size =
      match%bitstring bits with
      | {| value : size*8 : littleendian
	 ; rest   :    -1 : bitstring
        |}      -> value, rest
      | {| _ |} -> failwith "invalid varint"
    in
    match%bitstring bits with
    | {| tag  : 1*8 : littleendian
       ; rest :  -1 : bitstring
      |} -> begin
        match tag with
        | 0xff -> parse_value bits 8
        | 0xfe -> parse_value bits 4
        | 0xfd -> parse_value bits 2
        | i    -> Int64.of_int i, rest
      end
    | {| _ |} -> failwith "invalid varint"
  ;;

  let to_bitstring = function
    | i when i < 0xfdL ->
      let i = Int64.to_int_exn i in
      [%bitstring
        {| i : 1*8 : littleendian
        |}]
    | i when i < 0xffffL ->
      let i = Int64.to_int_exn i in
      [%bitstring
        {| 0xfd : 1*8
         ; i    : 2*8 : littleendian
        |}]
    | i when i < 0xffffffffL ->
      let i = Int64.to_int32_exn i in
      [%bitstring
        {| 0xfe : 1*8
         ; i    : 4*8 : littleendian
        |}]
    | i ->
      [%bitstring
        {| 0xff : 1*8
         ; i    : 8*8 : littleendian
        |}]
  ;;
end

module Varstring = struct
  type t = string [@@deriving compare, sexp]

  (* we should support strings with the full length of MAX(int64)
     bytes, but due to bitstring requiring the length in BITS in an
     OCaml int (31 bits, signed), we can only support much much
     shorter strings *facepalm* *)
  let of_bitstring ~name bits =
    let length, bits = Varint.of_bitstring bits in
    let length = Int64.to_int_exn length * 8 in
      match%bitstring bits with
      | {| value : length : string
	 ; rest  :     -1 : bitstring
        |} -> value, rest
      | {| _ |} -> failwithf "invalid %s" name ()
  ;;

  let to_bitstring s =
    let length =
      String.length s
      |> Int64.of_int
      |> Varint.to_bitstring
    in
    [%bitstring
      {| length : -1 : bitstring
       ; s      : -1 : string
      |}
    ]
  ;;
end

module Varlist = struct
  let fixed_length_string_of_bitstring ~name ~length = function%bitstring
    | {| value : length*8 : string
       ; rest  :       -1 : bitstring
      |} -> value, rest
    | {| _ |} -> failwithf "invalid %s list item" name ()
  ;;

  let fixed_length_string_to_bitstring ~length v =
    [%bitstring {| v : length*8 : string |}]
  ;;

  let of_bitstring ~element_of_bitstring bits =
    let rec elements acc bits = function
      | 0L -> acc, bits
      | n ->
        let element, bits = element_of_bitstring bits in
        elements (element :: acc) bits (Int64.pred n)
    in
    let count, bits = Varint.of_bitstring bits in
    let elements, rest = elements [] bits count in
    List.rev elements, rest
  ;;

  let to_bitstring ~bitstring_of_element t =
    let length =
      List.length t
      |> Int64.of_int
      |> Varint.to_bitstring
    in
    let elements =
      List.map t ~f:bitstring_of_element
      |> Bitstring.concat
    in
    [%bitstring
      {| length   : -1 : bitstring
       ; elements : -1 : bitstring
      |}]
  ;;
end
