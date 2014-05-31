let may f = function
  | None -> ()
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
