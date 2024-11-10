open CoreLib.TermTypes

(* open CoreLib.LambdaUtils *)
open CoreLib.TypingRules
(* open CoreLib.LambdaRules *)
(*
   let church_three =
     Abs ("f", Abs ("x", App (Var "f", App (Var "f", App (Var "f", Var "x"))))) *)

let lambda_term = App (Abs ("x", Var "x"), Abs ("x", Var "x"))
let _ = type_inference lambda_term
