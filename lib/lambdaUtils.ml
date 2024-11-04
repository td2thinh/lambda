open TermTypes
open ListUtils

let print_term (term : lambda_term) : string =
  let rec aux (term : lambda_term) : string =
    match term with
    | Var x -> x
    | Abs (x, t) -> Printf.sprintf "(λ%s.%s)" x (aux t)
    | App (t1, t2) -> Printf.sprintf "(%s %s)" (aux t1) (aux t2)
  in
  aux term

let variables (term : lambda_term) : string list =
  let rec aux (term : lambda_term) : string list =
    match term with
    | Var x -> [ x ]
    | Abs (x, t) -> union_list [ x ] (aux t)
    | App (t1, t2) -> union_list (aux t1) (aux t2)
  in
  aux term

let rec free_vars (term : lambda_term) : string list =
  match term with
  | Var x -> [ x ]
  | Abs (x, t) -> remove_elem (free_vars t) x
  | App (t1, t2) -> union_list (free_vars t1) (free_vars t2)

let bound_vars (term : lambda_term) : string list =
  minus_list (variables term) (free_vars term)

let equal (t1 : lambda_term) (t2 : lambda_term) : bool =
  let rec aux (t1 : lambda_term) (t2 : lambda_term) : bool =
    match (t1, t2) with
    | Var x, Var y -> x = y
    | Abs (x, t), Abs (y, t') -> x = y && aux t t'
    | App (t1, t2), App (t1', t2') -> aux t1 t1' && aux t2 t2'
    | _ -> false
  in
  aux t1 t2

let pp ppf term = Fmt.pf ppf "%s" (print_term term)

(* Alpha equivalence of 2 lambda terms *)
(* Code given by Rachid BOUHMAD *)
let alpha_equal t1 t2 =
  let rec alpha_eq env t1 t2 =
    match (t1, t2) with
    | Var x1, Var x2 -> (
        try List.assoc x1 env = x2 with Not_found -> x1 = x2)
    | Abs (x1, t1'), Abs (x2, t2') ->
        let new_env = (x1, x2) :: env in
        alpha_eq new_env t1' t2'
    | App (t1a, t1b), App (t2a, t2b) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b
    (* | Int n1, Int n2 -> n1 = n2
       | Add (t1a, t1b), Add (t2a, t2b) ->
           alpha_eq env t1a t2a && alpha_eq env t1b t2b
       | Sub (t1a, t1b), Sub (t2a, t2b) ->
           alpha_eq env t1a t2a && alpha_eq env t1b t2b *)
    | _ -> false
  in
  alpha_eq [] t1 t2

let rec print_type (t : lambda_type) : string =
  match t with
  | TVar x -> x
  | TArrow (t1, t2) ->
      Printf.sprintf "(%s -> %s)" (print_type t1) (print_type t2)
(* | TNat -> "Nat" *)

let type_equal (t1 : lambda_type) (t2 : lambda_type) : bool =
  let rec aux (t1 : lambda_type) (t2 : lambda_type) : bool =
    match (t1, t2) with
    | TVar x, TVar y -> x = y
    | TArrow (t1, t2), TArrow (t1', t2') -> aux t1 t1' && aux t2 t2'
    (* | TNat, TNat -> true *)
    | _ -> false
  in
  aux t1 t2
