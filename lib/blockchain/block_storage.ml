open! Core.Std
open Bitcaml_utils.Std
open Bitcoin_crypto.Std
open Bitcoin_protocol.Std

type t =
  { root_path     : string
  ; folder_levels : int
  } [@@deriving fields, sexp_of]

let check_random_folder t =
  let path =
    List.init (folder_levels t) ~f:(fun _ -> Random.int 255)
    |> List.map ~f:(Printf.sprintf "%02x")
    |> fun folders ->
    String.concat ~sep:"/" (root_path t :: folders)
  in
  try
    let stats = Unix.stat path in
    stats.Unix.st_kind = Unix.S_DIR
  with
  | Unix.Unix_error (Unix.ENOENT, "stat", _path) -> false
;;

let init t =
  let rec init_level path level =
    if level > 0 then
      for i = 0x00 to 0xff do
	let subpath = path ^/ (sprintf "%02x" i) in
	Unix.mkdir_p ~perm:0o755 subpath;
	init_level subpath (pred level)
      done
  in
  if not (check_random_folder t) then begin
    Unix.mkdir_p ~perm:0o755 (root_path t);
    init_level (root_path t) (folder_levels t)
  end
;;

let create
      ?(folder_levels=2)
      root_path
  =
  let t =
    Fields.create
      ~root_path
      ~folder_levels
  in
  init t;
  t
;;

let path_of_hash t hash =
  let rec path_of_hash_ hash acc = function
    | 0 -> List.rev acc
    | i ->
      let folder = String.sub hash ~pos:0 ~len:2 in
      let hash   = String.sub hash ~pos:2 ~len:(String.length hash - 2) in
      path_of_hash_ hash (folder :: acc)  (pred i)
  in
  let hash = Hash_string.to_string hash in
  let folders =
    path_of_hash_ hash [] (folder_levels t)
    |> String.concat ~sep:"/"
  in
  (root_path t) ^/ folders ^/ hash
;;

let path_of_block t block =
  Block.hash block
  |> path_of_hash t
;;

let store t block =
  let file_path = path_of_block t block in
  let bits = Block.to_bitstring block in
  Bitstring.bitstring_to_file bits file_path
;;

let load t hash =
  path_of_hash t hash
  |> Bitstring.bitstring_of_file
  |> Block.of_bitstring
  |> fst
;;
