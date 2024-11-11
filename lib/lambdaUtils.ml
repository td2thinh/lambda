open TermTypes

let print_term (term : lambda_term) : string =
  let rec aux (term : lambda_term) : string =
    match term with
    | Var x -> x
    | Abs (x, t) -> Printf.sprintf "(λ%s.%s)" x (aux t)
    | App (t1, t2) -> Printf.sprintf "(%s %s)" (aux t1) (aux t2)
    (* | App (t1, t2) -> (
           match t1 with
           | App (t1_1, t1_2) -> (
               match t1_1 with
               | Add (t1_1_1, t1_1_2) ->
                   Printf.sprintf "(%s + %s) %s" (aux t1_1_1) (aux t1_1_2)
                     (aux t1_2)
               | Sub (t1_1_1, t1_1_2) ->
                   Printf.sprintf "(%s - %s) %s" (aux t1_1_1) (aux t1_1_2)
                     (aux t1_2)
               | Cons (t1_1_1, t1_1_2) ->
                   Printf.sprintf "(%s :: %s) %s" (aux t1_1_1) (aux t1_1_2)
                     (aux t1_2)
               | _ -> Printf.sprintf "(%s %s)" (aux t1) (aux t2))
           | _ -> Printf.sprintf "(%s %s)" (aux t1) (aux t2))
       | IfZero (t1, t2, t3) ->
           Printf.sprintf "(if0 %s then %s else %s)" (aux t1) (aux t2) (aux t3)
       | IfEmpty (t1, t2, t3) ->
           Printf.sprintf "(ifE %s then %s else %s)" (aux t1) (aux t2) (aux t3)
       | List l -> Printf.sprintf "[%s]" (String.concat "; " (List.map aux l))
       | Cons (t1, t2) -> Printf.sprintf "(%s :: %s)" (aux t1) (aux t2)
       | Head t -> Printf.sprintf "head %s" (aux t)
       | Tail t -> Printf.sprintf "tail %s" (aux t)
       | Let (x, t1, t2) -> Printf.sprintf "let %s = %s in %s" x (aux t1) (aux t2)
       | Val n -> string_of_int n
       | Fix t -> Printf.sprintf "fix %s" (aux t)
       | Add (t1, t2) -> Printf.sprintf "(%s + %s)" (aux t1) (aux t2)
       | Sub (t1, t2) -> Printf.sprintf "(%s - %s)" (aux t1) (aux t2) *)
  in
  aux term

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
