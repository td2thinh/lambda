open TermTypes
open LambdaUtils
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
  | App (t1, t2) ->
      App (substitution var new_term t1, substitution var new_term t2)
  | Abs (x, t) ->
      if x = var then
        (* If bound variable is same as substitution variable, don't substitute *)
        term
      else
        (* Check if x appears free in new_term *)
        let new_term_free = free_vars new_term in
        if not (List.mem x new_term_free) then
          (* No variable capture possible, proceed with substitution *)
          Abs (x, substitution var new_term t)
        else
          (* Potential variable capture - perform alpha conversion first *)
          let new_var = fresh_var () in
          let renamed_body = substitution x (Var new_var) t in
          Abs (new_var, substitution var new_term renamed_body)

(* Beta reduction using the Left to Right - Call by Value strategy *)
(* Reduce to lambda expressions, only reduce Applications
   * reduce the left part of applications first and then right
   * ie. function part and then argument part *)
let rec ltr_cbv_step (term : lambda_term) : lambda_term option =
  (* print_endline ("Reducing: " ^ print_term term); *)
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
let ltr_cbv_norm (term : lambda_term) : (lambda_term, string) result =
  let term_alpha_converted = alpha_conversion term in
  let counter = ref 0 in
  let rec aux (term : lambda_term) : (lambda_term, string) result =
    if !counter >= max_steps then Error "Max reduction steps exceeded"
    else
      match ltr_cbv_step term with
      | Some t ->
          counter := !counter + 1;
          aux t
      | None -> Ok term
  in
  aux term_alpha_converted
