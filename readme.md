# Lambda Calculus Interpreter

This project aims to develop a somewhat sotisphicated Lambda Calculus based language.

# ROAD MAP:

## 1. Interpreting pure Lambda Calculus:
  
- alpha conversion : done
- left to right call by value evaluation : done
- encode I, δ, Ω, S, S K K, S I I, 0, 1, 2, 3, arithmetic operations : done
- tests : done

A Lambda expression is defined as :

```ocaml
type lambda_term =
  | Var of string
  | Abs of string * lambda_term
  | App of lambda_term * lambda_term
```
Evaluation strategy used is Left-to-Right Call by Value, evaluation rules for AST are as follows:

```ocaml
  | Var _ -> None
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
```

If the term is a variable, it returns None, if the term is an application of an abstraction to a term, it tries to reduce the term, if the term is an application of two terms, it tries to reduce the first term then the second term, if the term is an abstraction, it tries to reduce the term inside the abstraction.

 `I, δ, Ω, S, S K K, S I I, 0, 1, 2, 3, arithmetic operations` is encoded in `tests/ltRCbV.ml`. Tests in this file proved that everything is working as intended.

## 2. Simple Type:

- type equations : done
- type occurrence check : done
- typed substitution : done
- unification : done
- type inference : done
- tests : done 

A simple type is defined as : 

```ocaml
type lambda_type = TVar of string 
                | TArrow of lambda_type * lambda_type
```

A type equation is defined as :

```ocaml
type type_equation = (lambda_type * lambda_type) list
```

A type environment is defined as :

```ocaml
type type_env = (string * lambda_type) list
```

The inference algorithm is as follow :
- Generate a system of type equations from the lambda term
- Solve the system of type equations using unification with an empty environment
- If typable, the equations returned will be empty and the environment will contain the types of the variables, the function `substitute_type_env` substitutes the types of the variables in the lambda term with the types in the environment

The Unification algorithm is as follow :
- `unification` function takes a list of type equations and calls `unification_step` with a maximum number of steps of 300
- it uses a simple algorithm : for each equation in the list, it tries to unify the two types, if the two types are the same, it removes the equation from the list, if the two types are different, we use `occur_check` to see if it occurs in both sides of the equation (it shouldn't), we substitute the type variables for all the other equations using `substitute_type_all` and we add the new equations to the list, we do this until we can't unify anymore or we reach the maximum number of steps

- Tests with `identity`, `II` `apply`, `K`, `F`, `delta` `omega`, `S`, `S K K`, `S I I`, `S I I I`, `Triple X`, `K I Omega` proved that everything is working as intended.

## 3. Polymorphed Lambda Calculus with more Types

- Update lambda expression to include `Let`, `Integers`, `List`, `IfZero`, `IfEmpty`, `Cons`, `Head`, `Tail`, `Fix Point operator`, `Add`, `Subtract`, `Add` 
- Update all the functions to evaluate the new expressions

A Lambda expression is now defined as :

```ocaml
type lambda_term =
  | Var of string
  | Abs of string * lambda_term
  | App of lambda_term * lambda_term
  | Let of string * lambda_term * lambda_term
  | Val of int
  | Fix of lambda_term
  | Add of lambda_term * lambda_term
  | Mult of lambda_term * lambda_term
  | Sub of lambda_term * lambda_term
  | IfZero of lambda_term * lambda_term * lambda_term
  | IfEmpty of lambda_term * lambda_term * lambda_term
  | List of lambda_term list
  | Cons of lambda_term * lambda_term
  | Head of lambda_term
  | Tail of lambda_term
```

Updated alpha_conversion, substitution and left to right call by value evaluation functions to handle the new expressions.

```ocaml
.........
  | Let (x, t1, t2) -> (
      match ltr_cbv_step t1 with
      (* | Some t1' -> Some (Let (x, t1', t2)) *)
      (* Can't really reduce the let binding fully because it could be terms that are partially applied *)
      | _ -> Some (substitution x t1 t2))
  | Head (List l) -> ( match l with [] -> None | x :: _ -> Some x)
  | Tail (List l) -> ( match l with [] -> None | _ :: xs -> Some (List xs))
.........

 | Fix t -> (
      match t with
      | Abs (x, t') ->
          let alpha_renamed = alpha_conversion t' in
          Some (substitution x (Fix t) alpha_renamed)
      | _ -> (
          match ltr_cbv_step t with Some t' -> Some (Fix t') | None -> None))

...................
```
### Remarkable notes:

- Head and Tail on empty list will return Head (List []) and Tail (List []) respectively because it is not possible to evaluate them.
- In Let binding, I can't fully reduce the term because it could be terms that are partially applied, so I just substitute the variable in the term with the value of the let binding.
- In Point Fix operator, I first alpha rename the term and then substitute the variable in the term with the value of the let binding. It should only be lambda abstraction ie. functions but otherwise I will just reduce the term.

In the test file `tests/testPCF.ml`, I tested the following expressions :

`ex_plus_4_5` : 4 + 5 = 9

`ex_minus_4_5` : 4 - 5 = -1

`ex_list_4_5_6` : [4; 5; 6]

`cons_1_2_3` : (1 :: (2 :: 3))

`cons_1_2_3_eval` : [1; 2; 3] (Prints the list)

`ex_cons_456_123` : [4; 5; 6; 1; 2; 3] (Prints the list)

`ex_cons_123_456` : [1; 2; 3; 4; 5; 6] (Prints the list)

`if0_4_5_6` : if 4 = 0 then 5 else 6 = 6

`if0_0_5_6` : if 0 = 0 then 5 else 6 = 5

`ifempty_4_5_6` : if [4; 5] = [] then 5 else 6 = 6

`ifempty_4_5_6_empty` : if [] = [] then 5 else 6 = 5

`sum` : sum 10 = 55 (Using map to sum the list from 10 to 1)

`ex_mult_4_5` : 4 * 5 = 20

`ex_factoriel_5` : 5! = 120 

`ex_map_plus_1` : map (+1) [1; 2; 3; 4; 5] = [2; 3; 4; 5; 6]

`head` : head [1; 2; 3; 4; 5] = 1

`tail` : tail [1; 2; 3; 4; 5] = [2; 3; 4; 5]

`head_empty` : head [] = head []

`tail_empty` : tail [] = tail []

`let_x1_x2_plus_4_5` : let x1 = 4 in let x2 = 5 in x1 + x2 = 9

`let_map` : let f = (λx. x + 1) in map f [4; 5; 6] = [5; 6; 7]

`make_then_sum_list` : let make = (λx. if x = 0 then [] else x :: make (x - 1)) in sum (make 7) = 28

`sum_using_foldr` : let foldr = (λf. (λacc. (λl. if l = [] then acc else f (head l) (foldr f acc (tail l))))) in let sum = foldr (+) 0 in sum [1; 2; 3; 4; 5] = 15

All the tests passed successfully.

### Update the type for types to include `TNat` (Integers), `TList`, `TForAll` :

```ocaml
type lambda_type =
  | TVar of string
  | TArrow of lambda_type * lambda_type
  | TNat
  | TList of lambda_type
  | TForAll of string * lambda_type
```

### Updated the type inference algorithm to include the new types :

When a term is an operator, the target type is the type of the operator: 

Arithmetic operators : TNat -> TNat -> TNat

Head : TList A -> A

Tail : TList A -> TList A

Cons : A -> TList A -> TList A

IfZero : TNat -> A -> A -> A

IfEmpty : TList A -> B -> (TList A -> B) -> B

Fix : (A -> A) -> A

List : TList A and the type of the elements in the list is A

For Let binding, it's a little bit tricky because we introduce mutual recursion, so we need to infer the type of the first term, then we add the type of the variable to the environment and infer the type of the second term, then we remove the type of the variable from the environment.

```ocaml
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
  | Let (x, t1, t2) -> (
      (* On type e1 en utilisant type_inference *)
      match type_inference t1 with
      | Ok t0 ->
          (* On généralise le type t0 *)
          let generalized_t0 = generalize_type env t0 in
          (* On génère les équations pour e2 avec x:∀X1,...,Xk.T0 dans l'env *)
          let new_env = (x, generalized_t0) :: env in
          generate_equations t2 type_term new_env
      | Error e -> failwith ("Type error in let binding: " ^ e))
```
### IMPORTANT NOTE:

- While testing around with the type inference and let binding, I found out a HUGE error I was making that when trying to infer the type of `Let x = e1 in e2`, I infer the type of e1 like I'm supposed to but I'm inferring with an empty env, I modified the mutual recursive version of `type_inference` to take the env as a parameter so that I can pass the current env to the type inference of e1 and then continue like I'm supposed to. 

```ocaml
| Let (x, t1, t2) -> (
      let infered_t1 = type_inference_mutual_recursive t1 env in
      match infered_t1 with
      | Ok t1_type ->
          let generalized_t1 = generalize_type env t1_type in
          let new_env = (x, generalized_t1) :: env 
          generate_equations t2 type_term new_env
      | Error e -> failwith e)
................
and type_inference_mutual_recursive (term : lambda_term) (env : type_env) :
    (lambda_type, string) result
```

### Unification steps are updated to handle the new types:

TList A = TList B => A = B
TForAll x A = TForAll y B => A = B[x/y]

```ocaml
  | (TList t, TList t') :: xs ->
      let new_equations = (t, t') :: xs in
      unification_step new_equations env
  | (TForAll (_, t1), t2) :: xs | (t2, TForAll (_, t1)) :: xs ->
      let t1' = alpha_conversion_type t1 in
      let open_forall t = match t with TForAll (_, t) -> t | _ -> t in
      let new_equations = (open_forall t1', t2) :: xs in
      unification_step new_equations env
```


The test file `tests/typeInference2.ml` contains the following tests :

`ex_plus_4_5` : TNat

`ex_minus_4_5` : TNat

`factoriel` : TNat -> TNat

`ex_factoriel_5` : TNat

`ex_map_plus_1` : TNat

`head` : TNat

`tail` : TNat

`map_lambda_rec` : (A -> B) -> TList A -> TList B

`let_map` : (TNat -> TNat) -> TList TNat -> TList TNat

`sum_all_numbers_in_list` : TList TNat

`foldr` : (A -> B -> B) -> B -> TList A -> B

`ex_cons_1_2_3` : TList TNat

`make_number_list_function` : TNat -> TList TNat

`sum_all_numbers_in_list` : let make_number_list = fix (λmake_number_list.(λn.(if0 n then [] else (n :: (make_number_list (n - 1)))))) in let sum_list = fix (λsum_list.(λl.(ifE l then 0 else (head l + (sum_list tail l))))) in let list_1_2_3_4_5_6_7 = (make_number_list 7) in (sum_list list_1_2_3_4_5_6_7) : TNat (A function that makes a list of numbers from 1 to n and sums them)

All the tests passed successfully.

## 4. Imperative features
- Added `Ref` and `Assign` expressions to the lambda term, an expression is now: 
  
```ocaml
.....
| Unit
| Ref of lambda_term
| Deref of lambda_term
| Assign of lambda_term * lambda_term
| Region of int
```

- A memory region is its ID, a reference is a pointer to a memory region, a dereference is the value at the memory region, an assignment is the value at the memory region is assigned to the value of the expression, a region is a new memory region, defined as :
  
```ocaml
let state : (int * lambda_term) list ref = ref []
let lookup_region r = List.assoc r !state
let update_region r v = state := (r, v) :: List.remove_assoc r !state
```

- Updated the evaluation functions to handle the new expressions

```ocaml
.........
  | Unit -> None
  | Deref (Region id) -> (
      match lookup_state id !state with Some t -> Some t | None -> None)
  | Deref e -> (
      match ltr_cbv_step e with Some e' -> Some (Deref e') | None -> None)
  | Ref e -> (
      match ltr_cbv_step e with
      | Some e' -> Some (Ref e')
      | None ->
          (* Can't reduce <=> Val *)
          let id = fresh_region () in
          state := (id, e) :: !state;
          Some (Region id))
  | Assign (Var x, e) -> Some (Assign (Deref (Var x), e))
  | Assign (Deref (Region id), e) -> (
      match ltr_cbv_step e with
      | Some e' -> Some (Assign (Deref (Region id), e'))
      | None -> (
          match lookup_state id !state with
          | Some _ ->
              state := update_state id !state e;
              Some Unit
          | None -> None))
  | Assign (e1, e2) -> (
      match ltr_cbv_step e1 with
      | Some e1' -> Some (Assign (e1', e2))
      | None -> (
          match ltr_cbv_step e2 with
          | Some e2' -> Some (Assign (e1, e2'))
          | None -> None))
  | Region _ -> None
```

### Remarkable notes:
I changed back Let binding to try to reduce the e1 term before substituting it in the e2 term, I now have cases for those that can't be reduced.
```ocaml
  | Let (x, Fix (Abs (y, t1)), t2) ->
      Some (substitution x (Fix (Abs (y, t1))) t2)
  | Let (x, Abs (y, t1), t2) -> Some (substitution x (Abs (y, t1)) t2)
  | Let (x, t1, t2) -> (
      match ltr_cbv_step t1 with
      | Some t1' -> Some (Let (x, t1', t2))
      | _ -> Some (substitution x t1 t2))
```

Test file `tests/refAssign.ml` contains the following tests :

`assign_x_0_plus_1` : x = 0; x = x + 1; x -> 1

`update_list_value` : list = ref [1; 2] ; list := 3 :: 4 :: !list; !list -> [3; 4; 1; 2]

`let_counter` : let counter_fun = (λx. ref x) in let counter = counter_fun 0 in counter_fun + counter_fun + 8 -> 10

### Update the type for types to include `TRef` and `TUnit` :

```ocaml
type lambda_type =
  | TVar of string
  | TArrow of lambda_type * lambda_type
  | TNat
  | TList of lambda_type
  | TForAll of string * lambda_type
  | TUnit
  | TRef of lambda_type
```

Updated the type inference algorithm to include the new types :

```ocaml
  | Ref t -> [ (type_term, TRef t) ]
  | Deref (Region _) -> [ (type_term, TUnit) ]
  | Deref t -> generate_equations t (TRef type_term) env
  | Assign (t1, t2) -> (
      let equa1 = generate_equations t1 (TRef type_term) env in
      let equa2 = generate_equations t2 type_term env in
      equa1 @ equa2)
  | Region _ -> [ (type_term, TUnit) ]
```

Added 2 tests in test file `tests/typeInference2.ml`: 

`let_assign_x_0_plus_1` : x = 0; x = x + 1; x -> TNat

`let update_list_value` : list = ref [1; 2] ; list := 3 :: 4 :: !list; !list -> TList TNat

### Added Weak Polymporphism to the type inference algorithm :

```ocaml
type lambda_type =
  | TVar of string
  | TArrow of lambda_type * lambda_type
  | TNat
  | TList of lambda_type
  | TForAll of string * lambda_type
  | TUnit
  | TRef of lambda_type
  | TWeak of lambda_type
```

### Main changes include: 

Check if a term is non expansive
```ocaml
let is_non_expansive (term : lambda_term) : bool =
  match term with
  | Var _ | Abs _ | Val _ | Unit | Region _ | List _ -> true
  | _ -> false
```

If a term is non expansive, we can generalize it, otherwise it's weakly polymorphic
```ocaml
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
```

Let binding inference is now weakly polymorphic
```ocaml
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
```

### Added tests in test file `tests/weakPoly.ml`:

`term_1` : let f = (λx.x) in (f 3) -> TNat
`term_2` : let x = ref 3 in ((λy.!x) 4) -> TNat
`term_3` : let r = ref 0 in let f = (λx.!r) in ((f 1) + (f 42)) -> TNat
`term_4` : let r = ref 0 in let _ignored = r := 42 in let f = (λx.!r) in (f ()) -> TNat
`term_5` : let r = ref 0 in let g = (λx.let r2 = ref x in (!r + !r2)) in (g 1) -> TNat
# Parser
I'm trying to add a parser to the project, I'm using Menhir to generate the parser.

### Test for the parser is in `tests/parserTest.ml`:

parsing the map function is not working properly.

# Project Structure
```
lambda/
├── _build/                     # Build artifacts (generated by Dune)
├── bin/                        # Executable files
│   └── main.ml                 # Main entry point of the application
│   └── parser.mly              # Parser file
│   └── lexer.mll               # Lexer file
├── lib/                        # Library source files
│   ├── lambdaUtils.ml          # Utility functions for lambda calculus
│   ├── termTypes.ml            # Definitions of a lambda term and types
│   ├── lambdaRules.ml          # Reduction rules for lambda calculus
│   ├── typingRules.ml          # Type inference and unification rules
├── tests/                      # Test files
│   ├── alphaConv.ml            # Tests for alpha conversion
│   ├── ltRCbV.ml               # Tests for left-to-right call by value evaluation
│   ├── typeInference.ml        # Tests for type inference
│   ├── typeInference2.ml       # Tests for type inference with PCF types and operations
│   ├── testPCF.ml              # Tests for traits of PCF
│   ├── refAssign.ml            # Tests for ref and assign expressions
│   ├── weakPoly.ml             # Tests for weak polymorphism
│   ├── parserTest.ml           # Tests for the parser
├── dune                        # Dune build configuration file
├── dune-project                # Dune project file
├── lambda.opam                 # OPAM package file
├── Makefile                    # Makefile for building and running the project
├── README.md                   # Project README file
```

# Execution
Install the dependencies:

```bash
$ opam install . --deps-only
```

To run the project, you can use the following commands:

```bash
$ make run
```

To run the tests, you can use the following commands:

```bash
$ make test
```

# Dependencies
- dune
- alcotest
- fmt

# Notes:

- Free and Bound Variables codes: https://github.com/kmicinski/cmsc330examples

EDIT : I used to use these codes but I found out that I didn't need them so I removed them.

- Dune project example, alcotest: https://github.com/mjambon/dune-starter

- `CoreLib.LambdaUtils.alpha_equal` is copied from Rachid BOUHMAD

EDIT : In the early stages of the project, I copied his code but after I added my own code to it.

- Encoding arithmetic and church integers found at wiki: https://en.wikipedia.org/wiki/Lambda_calculus#Encoding_datatypes

- Idea to improve `CoreLib.LambdaRules.substitution` method found in : https://github.com/kmicinski/cmsc330examples as I was having some problem testing Church numerals, I found out that I need to check for free variables and alpha rename the variables in the substituting term in case there is variable capture (ie. variables of the same name but not the same). I also learned that I could have just alpha renamed my term every reduction step to avoid this problem.

EDIT: Finally, I chose to alpha rename the term every reduction step to avoid variable capture.

- While testing type inference I found out that K.I.ω = I but the algorithm will try to unify the equations and will fail, because ω is diverging so inference will fail with recursive types error but the term is actually well typed. I think this behavior is expected with the given algorithm in the project.

- I named the part 3 of the project PCF because I understood that it is a language that is based on lambda calculus and has some imperative features, I could be wrong.

- While testing around with the type inference and let binding, I found out a HUGE error I was making that when trying to infer the type of `Let x = e1 in e2`, I infer the type of e1 like I'm supposed to but I'm inferring with an empty env, I modified the mutual recursive version of `type_inference` to take the env as a parameter so that I can pass the current env to the type inference of e1 and then continue like I'm supposed to. 

EDIT: I moved this note above because it is a very important note.

- The parser is barely tested, for the moment, it works on basic expressions but I didn't test it on complex expressions. The point fix operator doesn't work, I'm trying to figure out how to parse recursive functions. Source for the barebone lambda calculus parser is from here : https://twolodzko.github.io/posts/ocaml-parser.html