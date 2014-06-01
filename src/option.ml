let may f = function
  | None -> ()
  | Some x -> f x
;;

let default default_value = function
  | None -> default_value
  | Some x -> x
;;

let default_f default_f f = function
  | None -> default_f ()
  | Some x -> f x
;;

let is_none = function
  | None -> true
  | Some x -> false
;;

exception No_value;;
let get = function
  | None -> raise No_value
  | Some x -> x
;;
