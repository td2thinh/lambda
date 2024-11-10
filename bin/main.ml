open CoreLib.TermTypes
open CoreLib.LambdaUtils
open CoreLib.TypingRules
(* open CoreLib.LambdaRules *)
(*
   let church_three =
     Abs ("f", Abs ("x", App (Var "f", App (Var "f", App (Var "f", Var "x"))))) *)

let identity = Abs ("x", Var "x")

let s_function =
  Abs
    ( "x",
      Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z"))))
    )
(* let ii = App (identity, identity)
   let apply = Abs ("x", Abs ("y", App (Var "x", Var "y")))
   let k_function = Abs ("x", Abs ("y", Var "x"))
   let f_function = Abs ("x", Abs ("y", Var "y"))


   let skk = App (App (s_function, k_function), k_function)
   let delta = Abs ("x", App (Var "x", Var "x"))
   let omega = App (delta, delta)
   let kiomega = App (App (k_function, identity), omega)
   let xxx = Abs ("x", App (App (Var "x", Var "x"), Var "x"))
   let siii = App (App (App (s_function, identity), identity), identity) *)

let sii = App (App (s_function, identity), identity)

(* let _ = print_endline (print_term identity)
   let _ = type_inference identity
   let _ = print_endline "-----------------------"
   let _ = print_endline (print_term ii)
   let _ = type_inference ii
   let _ = print_endline "-----------------------"
   let _ = print_endline (print_term apply)
   let _ = type_inference apply
   let _ = print_endline "-----------------------"
   let _ = print_endline (print_term k_function)
   let _ = type_inference k_function
   let _ = print_endline "-----------------------"
   let _ = print_endline (print_term f_function)
   let _ = type_inference f_function
   let _ = print_endline "-----------------------"
   let _ = print_endline (print_term s_function)
   let _ = type_inference s_function
   let _ = print_endline "-----------------------"
   let _ = print_endline (print_term skk)
   let _ = type_inference skk
   let _ = print_endline "-----------------------"
   let _ = print_endline (print_term delta)
   let _ = type_inference delta
   let _ = print_endline "-----------------------"
   let _ = print_endline (print_term omega)
   let _ = type_inference omega
   let _ = print_endline "-----------------------"
   let _ = print_endline (print_term sii) *)
let result = type_inference sii

let _ =
  match result with
  | Ok ty -> print_endline (print_type ty)
  | Error e -> print_endline e

(* let _ = print_endline "-----------------------"
   let _ = print_endline (print_term siii)
   let _ = type_inference siii
   let _ = print_endline "-----------------------"
   let _ = print_endline (print_term xxx)
   let _ = type_inference xxx
   let _ = print_endline "-----------------------"
   let _ = print_endline (print_term kiomega)
   let _ = type_inference kiomega *)
