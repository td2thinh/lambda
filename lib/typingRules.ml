open TermTypes
open LambdaUtils

let counter_var_type : int ref = ref 0

let fresh_var_type () : string =
  counter_var_type := !counter_var_type + 1;
  "T" ^ string_of_int !counter_var_type

type type_equation = (lambda_type * lambda_type) list

let print_equation (equation : type_equation) : string =
  let rec aux (equation : type_equation) : string =
    match equation with
    | [] -> ""
    | (t1, t2) :: xs ->
        Printf.sprintf "%s = %s\n" (print_type t1) (print_type t2) ^ aux xs
  in
  aux equation

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
  | TVar x -> if x = var then new_type else t
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

let print_env (env : type_env) : string =
  let rec aux (env : type_env) : string =
    match env with
    | [] -> ""
    | (x, t) :: xs -> Printf.sprintf "%s : %s\n" x (print_type t) ^ aux xs
  in
  aux env

let rec unification_step (equations : type_equation) (env : type_env) :
    (type_equation * type_env, string) result =
  match equations with
  | [] -> Ok ([], env)
  | (t1, t2) :: xs when t1 = t2 ->
      (* Printf.printf "Unifying %s and %s: already equal\n" (print_type t1)
         (print_type t2); *)
      unification_step xs env
  | (TVar x, t) :: xs ->
      if occur_check x t then Error "Type error: recursion"
      else
        let new_env = (x, t) :: env in
        let new_equations = substitute_type_all x t xs in
        (* Printf.printf "Substituting %s with %s\nNew equations: %s\n" x
           (print_type t)
           (print_equation new_equations); *)
        unification_step new_equations new_env
  | (t, TVar x) :: xs ->
      if occur_check x t then Error "Type error: recursion"
      else
        let new_env = (x, t) :: env in
        let new_equations = substitute_type_all x t xs in
        (* Printf.printf "Substituting %s with %s\nNew equations: %s\n" x
           (print_type t)
           (print_equation new_equations); *)
        unification_step new_equations new_env
  | (TArrow (t1, t2), TArrow (t1', t2')) :: xs ->
      let new_equations = [ (t1, t1'); (t2, t2') ] @ xs in
      (* Printf.printf
         "Decomposing arrow types: %s -> %s and %s -> %s\nNew equations: %s\n"
         (print_type t1) (print_type t2) (print_type t1') (print_type t2')
         (print_equation new_equations); *)
      unification_step new_equations env

let max_unification_steps = 300

let unification (equations : type_equation) (env : type_env) :
    (type_env, string) result =
  let counter = ref 0 in
  let rec aux (equations : type_equation) (env : type_env) :
      (type_env, string) result =
    if !counter >= max_unification_steps then
      Error "Max unification steps exceeded"
    else
      match unification_step equations env with
      | Ok ([], env) -> Ok env
      | Ok (new_equations, new_env) ->
          (* let _ =
               Printf.printf "Equations: %s\n" (print_equation new_equations)
             in *)
          counter := !counter + 1;
          aux new_equations new_env
      | Error e -> Error e
  in
  aux equations env

let rec substitute_type_env (env : type_env) (t : lambda_type) : lambda_type =
  match t with
  | TVar x -> (
      try substitute_type_env env (List.assoc x env) with Not_found -> TVar x)
  | TArrow (t1, t2) ->
      TArrow (substitute_type_env env t1, substitute_type_env env t2)

let type_inference (term : lambda_term) : (lambda_type, string) result =
  let new_var = fresh_var_type () in
  let type_var = TVar new_var in
  let env = [] in
  let equations = generate_equations term type_var env in
  match unification equations env with
  | Ok env ->
      (* let _ = Printf.printf "Environment: %s\n" (print_env env) in *)
      let result = substitute_type_env env type_var in
      (* let _ = Printf.printf "Result: %s\n" (print_type result) in *)
      Ok result
  | Error e -> Error e
