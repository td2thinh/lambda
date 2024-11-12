open CoreLib.TermTypes
open CoreLib.LambdaUtils

(* open CoreLib.TypingRules *)
open CoreLib.LambdaRules

let ex_plus_4_5 =
  App (App (Abs ("x", Abs ("y", Add (Var "x", Var "y"))), Val 4), Val 5)

let _ = print_endline (print_term ex_plus_4_5)

let _ =
  match ltr_cbv_norm ex_plus_4_5 with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e
