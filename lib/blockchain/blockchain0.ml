open! Core.Std
open Bignum.Std
open Bitcoin_crypto.Std
open Bitcoin_protocol.Std

module Node = struct
  type t =
    { height                : int64
    ; cumulative_difficulty : Bignum.t
    ; hash                  : Hash_string.t
    ; header                : Block.Header.t
    } [@@deriving bin_io, compare, fields, sexp]

  let equal a b = Hash_string.equal a.hash b.hash

  let create = Fields.create

  let of_previous ~header ~hash previous =
    let cumulative_difficulty =
      Block.Header.difficulty_target header
      |> Block.Difficulty.to_float
      |> Bignum.of_float
      |> Bignum.(+) previous.cumulative_difficulty
    in
    create
      ~height:(Int64.succ previous.height)
      ~cumulative_difficulty
      ~hash
      ~header
  ;;
end

module Orphan = struct
  type t =
    { hash : Hash_string.t
    ; header : Block.Header.t
    } [@@deriving bin_io, compare, fields, sexp]

  let equal a b = Hash_string.equal a.hash b.hash

  let create = Fields.create
end

type t =
  { genesis                     : Node.t
  ; mainchain_tip               : Node.t
  ; orphans                     : Orphan.t Hash_string.Map.t
  ; orphans_by_previous_block   : Hash_string.t list Hash_string.Map.t
  ; nodes                       : Node.t Hash_string.Map.t
  ; nodes_by_height             : Hash_string.t list Int64.Map.t
  ; unspent_transaction_outputs : Transaction.Output.t Transaction.Outpoint.Map.t
  } [@@deriving bin_io, compare, fields, sexp]

(* TODO: initialize with complete genesis block to obtain UTxO *)
let empty ~genesis_header ~genesis_hash =
  let genesis =
    let cumulative_difficulty =
      Block.Header.difficulty_target genesis_header
      |> Block.Difficulty.to_float
      |> Bignum.of_float
    in
    Node.create
      ~height:0L
      ~cumulative_difficulty
      ~hash:genesis_hash
      ~header:genesis_header
  in
  Fields.create
    ~genesis
    ~mainchain_tip:genesis
    ~orphans:Hash_string.Map.empty
    ~orphans_by_previous_block:Hash_string.Map.empty
    ~nodes:(Hash_string.Map.singleton genesis_hash genesis)
    ~nodes_by_height:(Int64.Map.singleton 0L [genesis_hash])
    ~unspent_transaction_outputs:Transaction.Outpoint.Map.empty
;;

let get_node t hash = Map.find t.nodes hash

let add_orphan t ~header ~hash =
  let orphans =
    Map.add
      t.orphans
      ~key:hash
      ~data:(Orphan.create ~header ~hash)
  in
  let orphans_by_previous_block =
    Map.add_multi
      t.orphans_by_previous_block
      ~key:(Block.Header.previous_block_hash header)
      ~data:hash
  in
  { t with orphans; orphans_by_previous_block }
;;

let add_node t node =
  let mainchain_tip =
    match
      Bignum.(>)
        (Node.cumulative_difficulty node)
        (Node.cumulative_difficulty t.mainchain_tip)
    with
    | false -> t.mainchain_tip
    | true  -> node
  in
  { t with
    mainchain_tip
  ; nodes = Map.add t.nodes ~key:(Node.hash node) ~data:node
  ; nodes_by_height = Map.add_multi t.nodes_by_height ~key:(Node.height node) ~data:(Node.hash node)
  }
;;

let rec resolve_orphans t added_nodes =
  List.concat_map added_nodes ~f:(fun node ->
    Map.find t.orphans_by_previous_block (Node.hash node)
    |> Option.value ~default:[]
    |> List.map ~f:(fun hash ->
      (node, hash)))
  |> List.filter_map ~f:(fun (previous_node, hash) ->
    Option.map (Map.find t.orphans hash) ~f:(fun orphan ->
      (previous_node, orphan)))
  |> List.fold ~init:(t, []) ~f:(fun (t, acc) (previous_node, orphan) ->
    let node =
      Node.of_previous
        ~header:(Orphan.header orphan)
        ~hash:(Orphan.hash orphan)
        previous_node
    in
    let t = add_node t node in
    (t, node :: acc))
  |> function
  | t, []          -> t
  | t, added_nodes -> resolve_orphans t added_nodes
;;

(* this assumes the block was verified, and thus is best effort
   instead of enforcing all inputs are unspent *)
let update_unspent_transaction_outputs t transaction =
  (* remove outputs being spent by this transactions inputs *)
  let hash = Transaction.hash transaction in
  let unspent_transaction_outputs =
    Transaction.inputs transaction
    |> Map.fold ~init:t.unspent_transaction_outputs ~f:(fun ~key:_ ~data:input map ->
      Map.remove map (Transaction.Input.previous_output input))
  in
  let unspent_transaction_outputs =
    Transaction.outputs transaction
    |> Map.fold ~init:unspent_transaction_outputs ~f:(fun ~key:index ~data:output map ->
      let outpoint =
        Transaction.Outpoint.create
          ~referenced_transaction_hash:hash
          ~index:(Int32.of_int_exn index)
      in
      Map.add map ~key:outpoint ~data:output)
  in
  { t with unspent_transaction_outputs }
;;

let insert' t ~header ~hash =
  match get_node t (Block.Header.previous_block_hash header) with
  | None               -> add_orphan t ~header ~hash
  | Some previous_node ->
    let node = Node.of_previous ~header ~hash previous_node in
    let t    = add_node t node in
    resolve_orphans t [node]
;;

let insert t block =
  let t =
    insert'
      t
      ~header:(Block.header block)
      ~hash:(Block.hash block)
  in
  List.fold (Block.transactions block) ~init:t ~f:update_unspent_transaction_outputs
;;
