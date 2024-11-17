open TermTypes
open LambdaUtils

let max_unification_steps = 300
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

let rec occur_check (var : string) (t : lambda_type) : bool =
  match t with
  | TVar x -> x = var
  | TArrow (t1, t2) -> occur_check var t1 || occur_check var t2
  | TNat -> false
  | TList t -> occur_check var t
  | TForAll (x, t) -> if x = var then false else occur_check var t
  | TUnit -> false
  | TRef t -> occur_check var t
  | TWeak t -> occur_check var t

let rec substitute_type (var : string) (new_type : lambda_type)
    (t : lambda_type) : lambda_type =
  match t with
  | TVar x -> if x = var then new_type else t
  | TArrow (t1, t2) ->
      TArrow (substitute_type var new_type t1, substitute_type var new_type t2)
  | TNat -> TNat
  | TList t -> TList (substitute_type var new_type t)
  | TForAll (x, t) ->
      if x = var then t else TForAll (x, substitute_type var new_type t)
  | TUnit -> TUnit
  | TRef t -> TRef (substitute_type var new_type t)
  | TWeak t -> TWeak (substitute_type var new_type t)

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

let rec substitute_type_env (env : type_env) (t : lambda_type) : lambda_type =
  match t with
  | TVar x -> (
      try substitute_type_env env (List.assoc x env) with Not_found -> TVar x)
  | TArrow (t1, t2) ->
      TArrow (substitute_type_env env t1, substitute_type_env env t2)
  | TNat -> TNat
  | TList t -> TList (substitute_type_env env t)
  | TForAll (x, t) -> TForAll (x, substitute_type_env env t)
  | TUnit -> TUnit
  | TRef t -> TRef (substitute_type_env env t)
  | TWeak t -> TWeak (substitute_type_env env t)

let rec free_type_variables (t : lambda_type) : string list =
  match t with
  | TVar x -> [ x ]
  | TArrow (t1, t2) -> free_type_variables t1 @ free_type_variables t2
  | TList t -> free_type_variables t
  | TForAll (x, t) -> List.filter (fun y -> y <> x) (free_type_variables t)
  | TNat -> []
  | TUnit -> []
  | TRef t -> free_type_variables t
  | TWeak t -> free_type_variables t

let generalize_type (env : type_env) (t : lambda_type) (is_non_expansive : bool)
    : lambda_type =
  let free_vars = free_type_variables t in
  let env_vars =
    List.fold_left (fun acc (_, typ) -> acc @ free_type_variables typ) [] env
  in
  let generalizable_vars =
    List.filter (fun v -> not (List.mem v env_vars)) free_vars
  in
  let generalizable_types =
    List.fold_right (fun v acc -> TForAll (v, acc)) generalizable_vars t
  in
  if is_non_expansive then generalizable_types else TWeak generalizable_types

let alpha_conversion_type (t : lambda_type) : lambda_type =
  let rec aux (t : lambda_type) (env : (string * string) list) : lambda_type =
    match t with
    | TVar x -> (
        match List.assoc_opt x env with Some y -> TVar y | None -> TVar x)
    | TArrow (t1, t2) -> TArrow (aux t1 env, aux t2 env)
    | TNat -> TNat
    | TList t -> TList (aux t env)
    | TForAll (x, t) ->
        let new_var = fresh_var_type () in
        let new_env = (x, new_var) :: env in
        TForAll (new_var, aux t new_env)
    | TUnit -> TUnit
    | TRef t -> TRef (aux t env)
    | TWeak t -> TWeak (aux t env)
  in
  aux t []

(* Only Forall is special, the rest is just the normal equal *)
let typeEqual (t1 : lambda_type) (t2 : lambda_type) : bool =
  match (t1, t2) with
  | TForAll (x1, t1), TForAll (x2, t2) ->
      let new_var = fresh_var_type () in
      let new_type = TVar new_var in
      let new_type1 = substitute_type x1 new_type t1 in
      let new_type2 = substitute_type x2 new_type t2 in
      if new_type1 = new_type2 then true else false
  | _ -> t1 = t2

let different_constructors t1 t2 =
  match (t1, t2) with
  | TVar _, _ | _, TVar _ -> false
  | TArrow _, TArrow _ -> false
  | TNat, TNat -> false
  | TList _, TList _ -> false
  | TForAll _, TForAll _ -> false
  | TUnit, TUnit -> false
  | TRef _, TRef _ -> false
  | TWeak _, TWeak _ -> false
  | _ -> true

let rec unification_step (equations : type_equation) (env : type_env) :
    (type_equation * type_env, string) result =
  match equations with
  | [] -> Ok ([], env)
  | (t1, t2) :: xs when typeEqual t1 t2 -> unification_step xs env
  | (TWeak t1, t2) :: xs | (t2, TWeak t1) :: xs ->
      unification_step ((t1, t2) :: xs) env
  | (TVar x, t) :: xs | (t, TVar x) :: xs ->
      if occur_check x t then
        Error
          (Printf.sprintf
             "Type error: recursive type detected when unifying %s with %s"
             (print_type (TVar x)) (print_type t))
      else
        let new_env = (x, t) :: env in
        let new_equations = substitute_type_all x t xs in
        unification_step new_equations new_env
  | (TArrow (t1, t2), TArrow (t1', t2')) :: xs ->
      let new_equations = (t1, t1') :: (t2, t2') :: xs in
      unification_step new_equations env
  | (TList t, TList t') :: xs ->
      let new_equations = (t, t') :: xs in
      unification_step new_equations env
  | (TForAll (_, t1), t2) :: xs | (t2, TForAll (_, t1)) :: xs ->
      let t1' = alpha_conversion_type t1 in
      let open_forall t = match t with TForAll (_, t) -> t | _ -> t in
      let new_equations = (open_forall t1', t2) :: xs in
      unification_step new_equations env
  | (TRef t1, TRef t2) :: xs -> unification_step ((t1, t2) :: xs) env
  | (t1, t2) :: _ when different_constructors t1 t2 ->
      Error
        (Printf.sprintf "Type error: cannot unify %s with %s, different types"
           (print_type t1) (print_type t2))
  | (t1, t2) :: _ ->
      Error
        (Printf.sprintf "Type error: cannot unify %s with %s" (print_type t1)
           (print_type t2))

let unification (equations : type_equation) (env : type_env) :
    (type_env, string) result =
  let counter = ref 0 in
  let rec aux (equations : type_equation) (env : type_env) :
      (type_env, string) result =
    let _ = Printf.printf "Equations: %s\n" (print_equation equations) in
    if !counter >= max_unification_steps then
      Error
        (Printf.sprintf
           "Maximum unification steps (%d) exceeded - possible infinite type"
           max_unification_steps)
    else
      match unification_step equations env with
      | Ok ([], env) -> Ok env
      | Ok (new_equations, new_env) ->
          counter := !counter + 1;
          aux new_equations new_env
      | Error e -> Error e
  in
  aux equations env

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
  | Fix (Abs (x, t)) ->
      let new_type1 = TVar (fresh_var_type ()) in
      let new_type2 = TVar (fresh_var_type ()) in
      let new_env = (x, TArrow (new_type1, new_type2)) :: env in
      let equa = generate_equations t (TArrow (new_type1, new_type2)) new_env in
      [ (type_term, TArrow (new_type1, new_type2)) ] @ equa
  | Val _ -> [ (type_term, TNat) ]
  | Add (t1, t2) | Mult (t1, t2) | Sub (t1, t2) ->
      let t1_equations = generate_equations t1 TNat env in
      let t2_equations = generate_equations t2 TNat env in
      t1_equations @ t2_equations @ [ (type_term, TNat) ]
  | Head t ->
      let new_var = fresh_var_type () in
      let new_type = TVar new_var in
      let equa = generate_equations t (TList new_type) env in
      equa @ [ (type_term, new_type) ]
  | Tail t ->
      let new_var = fresh_var_type () in
      let new_type = TVar new_var in
      let equa = generate_equations t (TList new_type) env in
      equa @ [ (type_term, TList new_type) ]
  | IfZero (t1, t2, t3) ->
      let equa_condition = generate_equations t1 TNat env in
      let equa_consequent = generate_equations t2 type_term env in
      let equa_alternant = generate_equations t3 type_term env in
      equa_condition @ equa_consequent @ equa_alternant
  | IfEmpty (t1, t2, t3) ->
      let new_var = fresh_var_type () in
      let type_var = TList (TVar new_var) in
      let equa_condition = generate_equations t1 type_var env in
      let equa_consequent = generate_equations t2 type_term env in
      let equa_alternant = generate_equations t3 type_term env in
      equa_condition @ equa_consequent @ equa_alternant
  | List l ->
      let new_var = fresh_var_type () in
      let type_var = TVar new_var in
      let equa = [ (type_term, TList type_var) ] in
      List.flatten (List.map (fun x -> generate_equations x type_var env) l)
      @ equa
  | Cons (t1, t2) ->
      let new_var = fresh_var_type () in
      let type_var = TList (TVar new_var) in
      let equa_head = generate_equations t1 (TVar new_var) env in
      let equa_tail = generate_equations t2 type_var env in
      equa_head @ equa_tail @ [ (type_term, type_var) ]
  | Unit -> [ (type_term, TUnit) ]
  | Ref t ->
      let new_var = fresh_var_type () in
      let type_var = TVar new_var in
      let equa = generate_equations t type_var env in
      equa @ [ (type_term, TRef type_var) ]
  | Deref t ->
      let new_var = fresh_var_type () in
      let type_var = TVar new_var in
      let equa = generate_equations t (TRef type_var) env in
      equa @ [ (type_term, type_var) ]
  | Assign (t1, t2) ->
      let new_var = fresh_var_type () in
      let type_var = TVar new_var in
      let put_weak_types (e : type_env) : type_env =
        List.map
          (fun (v, t) ->
            let t' = match t with TWeak t -> t | _ -> t in
            (v, t'))
          e
      in
      let new_env = put_weak_types env in
      let equa1 = generate_equations t1 (TRef type_var) new_env in
      let equa2 = generate_equations t2 type_var new_env in
      equa1 @ equa2 @ [ (type_term, TUnit) ]
  | Region _ -> [ (type_term, TUnit) ]
  | Let (x, t1, t2) -> (
      let infered_t1 = type_inference_mutual_recursive t1 env in
      match infered_t1 with
      | Ok t1_type ->
          let generalized_t1 =
            generalize_type env t1_type (is_non_expansive t1)
          in
          let new_env = (x, generalized_t1) :: env in
          generate_equations t2 type_term new_env
      | Error e -> failwith e)
  | _ -> failwith "Not implemented"

and type_inference_mutual_recursive (term : lambda_term) (env : type_env) :
    (lambda_type, string) result =
  let new_var = fresh_var_type () in
  let type_var = TVar new_var in
  let equations = generate_equations term type_var env in
  match unification equations env with
  | Ok env ->
      let result = substitute_type_env env type_var in
      Ok result
  | Error e -> Error e

let type_inference (term : lambda_term) : (lambda_type, string) result =
  let new_var = fresh_var_type () in
  let type_var = TVar new_var in
  let env = [] in
  let equations = generate_equations term type_var env in
  match unification equations env with
  | Ok env ->
      let result = substitute_type_env env type_var in
      Ok result
  | Error e -> Error e
