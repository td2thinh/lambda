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
  | Unit
  | Ref of lambda_term
  | Deref of lambda_term
  | Assign of lambda_term * lambda_term
  | Region of int

type lambda_type =
  | TVar of string
  | TArrow of lambda_type * lambda_type
  | TNat
  | TList of lambda_type
  | TForAll of string * lambda_type
