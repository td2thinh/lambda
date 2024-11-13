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
Evaluation strategy used is Left-to-Right Call by Value, `I, δ, Ω, S, S K K, S I I, 0, 1, 2, 3, arithmetic operations` is encoded in `tests/ltRCbV.ml`. Tests in this file proved that everything is working as intended.

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

All the tests passed successfully.



## 4. Imperative features

# Project Structure
```
lambda/
├── _build/                     # Build artifacts (generated by Dune)
├── bin/                        # Executable files
│   └── main.ml                 # Main entry point of the application
├── lib/                        # Library source files
│   ├── lambdaUtils.ml          # Utility functions for lambda calculus
│   ├── termTypes.ml            # Definitions of a lambda term and types
│   ├── lambdaRules.ml          # Reduction rules for lambda calculus
│   ├── typingRules.ml          # Type inference and unification rules
├── tests/                      # Test files
│   ├── alphaConv.ml            # Tests for alpha conversion
│   ├── ltRCbV.ml               # Tests for left-to-right call by value evaluation
│   ├── typeInference.ml        # Tests for type inference
│   ├── testPCF.ml              # Tests for traits of PCF
├── dune                        # Dune build configuration file
├── dune-project                # Dune project file
├── lambda.opam                 # OPAM package file
├── Makefile                    # Makefile for building and running the project
├── README.md                   # Project README file
└── test.txt                    # This file contains the type inference with prints
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