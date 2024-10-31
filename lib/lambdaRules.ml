open TermTypes
module L = List
module M = Map

let counter_var : int ref = ref 0

let fresh_var () : string =
  counter_var := !counter_var + 1;
  "X" ^ string_of_int !counter_var

module StringMap = M.Make (String)

let alpha_conversion (term : lambda_term) : lambda_term =
  let var_map = StringMap.empty in
  let rec aux (term : lambda_term) (var_map : string StringMap.t) : lambda_term
      =
    match term with
    | Var x -> (
        match StringMap.find_opt x var_map with
        | Some y -> Var y
        | None -> Var x)
    | Abs (x, t) ->
        let new_var = fresh_var () in
        Abs (new_var, aux t (StringMap.add x new_var var_map))
    | App (t1, t2) -> App (aux t1 var_map, aux t2 var_map)
  in
  aux term var_map

(* Subsitute all free variables by a new term *)
let rec substitution (var : string) (new_term : lambda_term)
    (term : lambda_term) : lambda_term =
  match term with
  | Var x -> if x = var then new_term else Var x
  | Abs (x, t) -> Abs (x, substitution var new_term t)
  | App (t1, t2) ->
      App (substitution var new_term t1, substitution var new_term t2)

(* Beta reduction using the Left to Right - Call by Value strategy *)
(* Reduce to lambda expressions, only reduce Applications
   * reduce the left part of applications first and then right
   * ie. function part and then argument part *)
let rec ltr_cbv_step (term : lambda_term) : lambda_term option =
  match term with
  | App (Abs (x, t), t') -> (
      match ltr_cbv_step t' with
      | Some t2' -> Some (App (Abs (x, t), t2'))
      | None -> Some (substitution x t' t))
  | App (t1, t2) -> (
      match ltr_cbv_step t1 with
      | Some t1' -> Some (App (t1', t2))
      | None -> (
          match ltr_cbv_step t2 with
          | Some t2' -> Some (App (t1, t2'))
          | None -> None))
  | Abs (x, t) -> (
      match ltr_cbv_step t with Some t' -> Some (Abs (x, t')) | None -> None)
  | _ -> None

let max_steps = 300

(* Normalize a lambda term using the Left to Right - Call by Value strategy *)
let ltr_cbv_norm (term : lambda_term) : lambda_term option =
  let term_alpha_converted = alpha_conversion term in
  let counter = ref 0 in
  let rec aux (term : lambda_term) : lambda_term option =
    if !counter >= max_steps then None
    else
      match ltr_cbv_step term with
      | Some t ->
          counter := !counter + 1;
          aux t
      | None -> Some term
  in
  aux term_alpha_converted
