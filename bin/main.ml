open CoreLib.TermTypes
open CoreLib.LambdaUtils

(* open CoreLib.LambdaRules *)
(*
   let church_three =
     Abs ("f", Abs ("x", App (Var "f", App (Var "f", App (Var "f", Var "x"))))) *)

let true_lambda = Abs ("x", Abs ("y", Var "x"))
let false_lambda = Abs ("x", Abs ("y", Var "y"))

let is_zero =
  Abs ("n", App (App (Var "n", Abs ("x", true_lambda)), false_lambda))

let _ = print_endline (print_term is_zero)
