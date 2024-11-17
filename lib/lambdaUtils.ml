open TermTypes

let print_term (term : lambda_term) : string =
  let rec aux (term : lambda_term) : string =
    match term with
    | Var x -> x
    | Abs (x, t) -> Printf.sprintf "(λ%s.%s)" x (aux t)
    | App (t1, t2) -> (
        match t1 with
        | App (t1_1, _) -> (
            match t1_1 with
            | Add (_, t_2_2) -> Printf.sprintf "(%s + %s)" (aux t_2_2) (aux t2)
            | Sub (_, t_2_2) -> Printf.sprintf "(%s - %s)" (aux t_2_2) (aux t2)
            | Mult (_, t_2_2) -> Printf.sprintf "(%s * %s)" (aux t_2_2) (aux t2)
            | Cons (_, t_2_2) ->
                Printf.sprintf "(%s :: %s)" (aux t_2_2) (aux t2)
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
    | Sub (t1, t2) -> Printf.sprintf "(%s - %s)" (aux t1) (aux t2)
    | Mult (t1, t2) -> Printf.sprintf "(%s * %s)" (aux t1) (aux t2)
    | Unit -> "()"
    | Ref t -> Printf.sprintf "ref %s" (aux t)
    | Deref t -> Printf.sprintf "!%s" (aux t)
    | Assign (t1, t2) -> Printf.sprintf "%s := %s" (aux t1) (aux t2)
    | Region n -> Printf.sprintf "region #%d" n
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
    (* From here on out it's my code *)
    | Let (x1, t1', t1''), Let (x2, t2', t2'') ->
        alpha_eq env t1' t2' && alpha_eq ((x1, x2) :: env) t1'' t2''
    | Val n1, Val n2 -> n1 = n2
    | Fix t1', Fix t2' -> alpha_eq env t1' t2'
    | Add (t1a, t1b), Add (t2a, t2b) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b
    | Mult (t1a, t1b), Mult (t2a, t2b) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b
    | Sub (t1a, t1b), Sub (t2a, t2b) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b
    | IfZero (t1a, t1b, t1c), IfZero (t2a, t2b, t2c) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b && alpha_eq env t1c t2c
    | IfEmpty (t1a, t1b, t1c), IfEmpty (t2a, t2b, t2c) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b && alpha_eq env t1c t2c
    | List l1, List l2 ->
        List.length l1 = List.length l2
        && List.for_all2 (fun x y -> alpha_eq env x y) l1 l2
    | Cons (t1a, t1b), Cons (t2a, t2b) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b
    | Head t1, Head t2 -> alpha_eq env t1 t2
    | Tail t1, Tail t2 -> alpha_eq env t1 t2
    | Unit, Unit -> true
    | Ref t1, Ref t2 -> alpha_eq env t1 t2
    | Deref t1, Deref t2 -> alpha_eq env t1 t2
    | Assign (t1a, t1b), Assign (t2a, t2b) ->
        alpha_eq env t1a t2a && alpha_eq env t1b t2b
    | Region n1, Region n2 -> n1 = n2
    | _ -> false
  in
  alpha_eq [] t1 t2

let rec print_type (t : lambda_type) : string =
  match t with
  | TVar x -> x
  | TArrow (t1, t2) ->
      Printf.sprintf "(%s -> %s)" (print_type t1) (print_type t2)
  | TNat -> "Nat"
  | TList t -> Printf.sprintf "[%s]" (print_type t)
  | TForAll (x, t) -> Printf.sprintf "∀%s.%s" x (print_type t)
  | TUnit -> "Unit"
  | TRef t -> Printf.sprintf "Ref %s" (print_type t)
  | TWeak t -> Printf.sprintf "Weak _%s" (print_type t)

let pp_type ppf t = Fmt.pf ppf "%s" (print_type t)

let alpha_equal_type t1 t2 =
  let map = ref [] in
  let rec aux t1 t2 =
    match (t1, t2) with
    | TVar x1, TVar x2 -> (
        try List.assoc x1 !map = x2 with Not_found -> x1 = x2)
    | TArrow (t1a, t1b), TArrow (t2a, t2b) -> aux t1a t2a && aux t1b t2b
    | TNat, TNat -> true
    | TList t1', TList t2' -> aux t1' t2'
    | TForAll (x1, t1'), TForAll (x2, t2') ->
        map := (x1, x2) :: !map;
        aux t1' t2'
    | TUnit, TUnit -> true
    | TRef t1', TRef t2' -> aux t1' t2'
    | TWeak t1', TWeak t2' -> aux t1' t2'
    | _ -> false
  in
  aux t1 t2

let is_non_expansive (term : lambda_term) : bool =
  match term with
  | Var _ | Abs _ | Val _ | Unit | Region _ | List _ -> true
  | _ -> false
