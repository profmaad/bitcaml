open! Core.Std
open Bitcaml_utils.Std

type t = Word.t list [@@deriving bin_io, compare, sexp]

let sigop_count t =
  let constant_pusher_value opcode =
    if (opcode >= 0x51) && (opcode <= 0x60) then
      opcode - 0x50
    else
      20
  in
  let rec sigop_count_acc acc word =
    match (word : Word.t list) with
    | [] ->
      acc
    | CheckSig :: script
    | CheckSigVerify :: script ->
      sigop_count_acc (acc + 1) script
    | (Data {opcode; data = _}) :: CheckMultiSig :: script
    | (Data {opcode; data = _}) :: CheckMultiSigVerify :: script ->
      sigop_count_acc (acc + (constant_pusher_value opcode)) script
    | CheckMultiSig :: script
    | CheckMultiSigVerify :: script ->
      sigop_count_acc (acc + 20) script
    | _ :: script ->
      sigop_count_acc acc script
  in
  sigop_count_acc 0 t
;;

let to_string_hum t =
  let header = sprintf "Bitcoin Transaction Script (%u words):" (List.length t) in
  let words =
    List.mapi t ~f:(fun i word ->
      sprintf !"% 5u: %{Word#hum}" i word)
  in
  String.concat ~sep:"\n\t" (header :: words)
;;

let of_bitstring bits =
  let rec of_bitstring' acc bits =
    match Bitstring.bitstring_length bits with
    | 0 -> List.rev acc, bits
    | _ ->
      let word, bits = Word.of_bitstring bits in
      of_bitstring' (word :: acc) bits
  in
  of_bitstring' [] bits
;;

let to_bitstring t =
  List.map t ~f:Word.to_bitstring
  |> Bitstring.concat
;;

let%test_unit "round trip" =
  let bits =
    Bitstring.of_string
      "\x76\xa9\x14\x2f\xef\x8e\xdc\xc4\x50\x19\xac\xba\x3b\xb1\x46\xb7\x6c\xbd\x2f\x84\x8b\xe5\xd6\x88\xac"
  in
  let parsed, rest = of_bitstring bits in
  [%test_result: int] ~expect:0 (Bitstring.bitstring_length rest);
  [%test_result: t] ~expect:[] parsed;
  [%test_result: Bitstring.t] ~expect:bits (to_bitstring parsed)
;;
