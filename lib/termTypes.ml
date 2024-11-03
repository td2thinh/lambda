type lambda_term =
  | Var of string
  | Abs of string * lambda_term
  | App of lambda_term * lambda_term

type lambda_type = TVar of string | TArrow of lambda_type * lambda_type
(* | TNat *)
