open TermTypes

let print_term (term : lambda_term) : string =
  let rec aux (term : lambda_term) : string =
    match term with
    | Var x -> x
    | Abs (x, t) -> Printf.sprintf "(λ%s.%s)" x (aux t)
    | App (t1, t2) -> Printf.sprintf "(%s %s)" (aux t1) (aux t2)
  in
  aux term

(*  function to return all the variables in a lambda term *)
let variables (term : lambda_term) : string list =
  let rec aux (term : lambda_term) : string list =
    match term with
    | Var x -> [ x ]
    | Abs (x, t) -> List.filter (fun y -> y <> x) (aux t)
    | App (t1, t2) -> List.rev_append (aux t1) (aux t2)
  in
  List.sort_uniq String.compare (aux term)

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
