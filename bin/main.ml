open CoreLib.TermTypes
open CoreLib.LambdaUtils

(* open CoreLib.TypingRules *)
open CoreLib.LambdaRules

let ex_plus_4_5 =
  App (App (Abs ("x", Abs ("y", Add (Var "x", Var "y"))), Val 4), Val 5)

let ex_minus_4_5 =
  App (App (Abs ("x", Abs ("y", Sub (Var "x", Var "y"))), Val 4), Val 5)

let ex_list_4_5_6 = List [ Val 4; Val 5; Val 6 ]
let ex_cons_1_2_3 = Cons (Val 1, Cons (Val 2, Val 3))
let if0_4_5_6 = IfZero (Val 0, Val 5, Val 6)
let ifempty_4_5_6 = IfEmpty (ex_list_4_5_6, Val 5, Val 6)

let sum_3_rec =
  Fix
    (Abs
       ( "sum",
         Abs
           ( "n",
             IfZero
               ( Var "n",
                 Val 0,
                 Add (Var "n", App (Var "sum", Sub (Var "n", Val 1))) ) ) ))

let test_sum = App (sum_3_rec, Val 10)

let _ =
  match ltr_cbv_norm ex_plus_4_5 with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e

let _ =
  match ltr_cbv_norm ex_minus_4_5 with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e

let _ =
  match ltr_cbv_norm ex_list_4_5_6 with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e

let _ =
  match ltr_cbv_norm ex_cons_1_2_3 with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e

let _ =
  match ltr_cbv_norm if0_4_5_6 with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e

let _ =
  match ltr_cbv_norm ifempty_4_5_6 with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e

let _ =
  match ltr_cbv_norm test_sum with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e
