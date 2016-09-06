open! Core.Std
open Bitcoin_protocol;;

let folder_levels = 2;;

type t = {
  root_path : string;
  folder_levels : int;
};;

let check_random_folder storage =
  let path = String.concat ~sep:"/" (storage.root_path :: (List.map ~f:(Printf.sprintf "%02x") (Utils.random_list 255 storage.folder_levels))) in

  try
    let stats = Unix.stat path in
    stats.Unix.st_kind = Unix.S_DIR
  with
  | Unix.Unix_error (Unix.ENOENT, "stat", path) -> false
;;

let init storage =
  let rec init_level path level =
    if level > 0 then
      for i = 0x00 to 0xff do
	let subpath = (path ^ (Printf.sprintf "%02x/" i)) in
	Utils.mkdir_maybe subpath 0o755;
	init_level subpath (level - 1);
      done;
  in
  let path = storage.root_path in
  let path = (if storage.root_path.[(String.length path - 1)] = '/' then path else path ^ "/") in
  Utils.mkdir_maybe path 0o755;
  init_level path storage.folder_levels
;;
let maybe_init storage =
  if not (check_random_folder storage) then
    init storage;
;;

let init_default path =
  let storage = {
    root_path = path;
    folder_levels = folder_levels;
  }
  in
  maybe_init storage;
  storage
;;

let path_of_hash storage hash =
  let rec path_of_hash_ hash levels =
    if levels = 0 then []
    else
      (String.sub hash 0 2) :: (path_of_hash_ (String.sub hash 2 ((String.length hash) - 2)) (levels - 1))
  in
  storage.root_path ^ (String.concat ~sep:"/" (path_of_hash_ hash storage.folder_levels)) ^ "/" ^ hash
;;
let path_of_block storage block =
  let hash = Utils.hex_encode (Bitcoin_protocol_generator.block_hash block.block_header) in
  path_of_hash storage hash
;;

let store_block storage block =
  let file_path = path_of_block storage block in
  Bitstring.bitstring_to_file (Bitcoin_protocol_generator.bitstring_of_block block) file_path
;;
let load_block storage hash =
  let file_path = path_of_hash storage hash in
  let block_bitstring = Bitstring.bitstring_of_file file_path in
  let block, _ = Bitcoin_protocol_parser.parse_block block_bitstring in
  block
;;
