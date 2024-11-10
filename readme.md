# Lambda Calculus Interpreter

This project aims to develop a somewhat sotisphicated Lambda Calculus based language.

# ROAD MAP:

1. Interpreting pure Lambda Calculus:
  
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

2. Simple Type:

- type equations : done
- type occurrence check : done
- typed substitution : done
- unification : done
- type inference : done
- tests 

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

3. Polymorphed Lambda Calculus with more Types
4. Imperative features

# SOURCES:

- Free and Bound Variables codes: https://github.com/kmicinski/cmsc330examples

- Dune project example, alcotest: https://github.com/mjambon/dune-starter

- `CoreLib.LambdaUtils.alpha_equal` is copied from Rachid BOUHMAD
- Encoding arithmetic and church integers found at wiki: https://en.wikipedia.org/wiki/Lambda_calculus#Encoding_datatypes

- Idea to improve `CoreLib.LambdaRules.substitution` method found in : https://github.com/kmicinski/cmsc330examples as I was having some problem testing Church numerals, I found out that I need to check for free variables and alpha rename the variables in the substituting term in case there is variable capture (ie. variables of the same name but not the same). I also learned that I could have just alpha renamed my term every reduction step to avoid this problem.
