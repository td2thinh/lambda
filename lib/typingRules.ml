open TermTypes

let counter_var_type : int ref = ref 0

let fresh_var_type () : string =
  counter_var_type := !counter_var_type + 1;
  "T" ^ string_of_int !counter_var_type

type type_equation = (lambda_type * lambda_type) list
type type_env = (string * lambda_type) list

let rec search_type (var : string) (env : type_env) : lambda_type option =
  match env with
  | [] -> None
  | (x, t) :: xs -> if x = var then Some t else search_type var xs

let rec generate_equations (term : lambda_term) (type_term : lambda_type)
    (env : type_env) : type_equation =
  match term with
  | Var x -> (
      match search_type x env with
      | Some t -> [ (type_term, t) ]
      | None -> failwith "Variable not found in environment")
  | Abs (x, t) ->
      let new_var_1 = fresh_var_type () in
      let new_var_2 = fresh_var_type () in
      let type_var = TVar new_var_1 in
      let type_body = TVar new_var_2 in
      let new_env = (x, type_var) :: env in
      let equa = [ (type_term, TArrow (type_var, type_body)) ] in
      let equa_body = generate_equations t type_body new_env in
      equa @ equa_body
  | App (t1, t2) ->
      let new_var = fresh_var_type () in
      let type_var = TVar new_var in
      let equa_function =
        generate_equations t1 (TArrow (type_var, type_term)) env
      in
      let equa_argument = generate_equations t2 type_var env in
      equa_function @ equa_argument

let rec occur_check (var : string) (t : lambda_type) : bool =
  match t with
  | TVar x -> x = var
  | TArrow (t1, t2) -> occur_check var t1 || occur_check var t2
(* | TNat -> false *)

let rec substitute_type (var : string) (new_type : lambda_type)
    (t : lambda_type) : lambda_type =
  match t with
  | TVar x -> if x = var then new_type else TVar x
  | TArrow (t1, t2) ->
      TArrow (substitute_type var new_type t1, substitute_type var new_type t2)
(* | TNat -> TNat *)

let rec substitute_type_all (var : string) (new_type : lambda_type)
    (equations : type_equation) : type_equation =
  match equations with
  | [] -> []
  | (t1, t2) :: xs ->
      let new_t1 = substitute_type var new_type t1 in
      let new_t2 = substitute_type var new_type t2 in
      (new_t1, new_t2) :: substitute_type_all var new_type xs

let max_unification_steps = 300

let unification_step (equations : type_equation) : type_equation =
  let current_count = ref 0 in
  let rec aux (equations : type_equation) : type_equation =
    if !current_count >= max_unification_steps then failwith "Max steps reached"
    else
      match equations with
      | [] -> []
      | (t1, t2) :: xs -> (
          current_count := !current_count + 1;
          match (t1, t2) with
          | TVar x, t ->
              if not (occur_check x t) then
                (t1, t2) :: aux (substitute_type_all x t equations)
              else failwith "Type variable occurs in the type itself"
          | t, TVar x ->
              if not (occur_check x t) then
                (t1, t2) :: aux (substitute_type_all x t equations)
              else failwith "Type variable occurs in the type itself"
          | TArrow (t1, t2), TArrow (t1', t2') ->
              aux ((t1, t1') :: (t2, t2') :: xs))
  in
  aux equations
