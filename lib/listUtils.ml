(* List util functions *)
module L = List

(* Return a list of unique members *)
let unique l =
  L.fold_right (fun x acc -> if L.mem x acc then acc else x :: acc) l []

(* Union of 2 lists *)
let union_list l1 l2 = unique (l1 @ l2)

(* Return list1 \ list2 *)
let minus_list l1 l2 = L.filter (fun x -> not (L.mem x l2)) l1

(* Remove a elem from list *)
let remove_elem l x = L.filter (fun y -> y <> x) l
