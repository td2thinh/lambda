open CoreLib.TermTypes
open CoreLib.LambdaUtils
open CoreLib.TypingRules
open CoreLib.LambdaRules

(* let x = ref 0 in x := (x + 1) in x *)
let let_assign_x_0_plus_1 =
  Let
    ( "x",
      Ref (Val 0),
      Let
        ( "_",
          Assign (Deref (Var "x"), Add (Deref (Var "x"), Val 1)),
          Deref (Var "x") ) )

let _ =
  match ltr_cbv_norm let_assign_x_0_plus_1 with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e

let foldr =
  Fix
    (Abs
       ( "f",
         Abs
           ( "g",
             Abs
               ( "acc",
                 Abs
                   ( "xs",
                     IfEmpty
                       ( Var "xs",
                         Var "acc",
                         App
                           ( App (Var "g", Head (Var "xs")),
                             App
                               ( App (App (Var "f", Var "g"), Var "acc"),
                                 Tail (Var "xs") ) ) ) ) ) ) ))

let sum_using_fold_right =
  Abs
    ( "l",
      App
        ( App
            ( App (foldr, Abs ("x", Abs ("acc", Add (Var "x", Var "acc")))),
              Val 0 ),
          Var "l" ) )

let ex_fold_right =
  Let
    ( "foldr",
      foldr,
      Let
        ( "list_1_2_3_4_5",
          List [ Val 1; Val 2; Val 3; Val 4; Val 5 ],
          Let
            ("sum", sum_using_fold_right, App (Var "sum", Var "list_1_2_3_4_5"))
        ) )

let _ =
  match ltr_cbv_norm ex_fold_right with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e

let _ =
  match type_inference ex_fold_right with
  | Ok t -> print_endline (print_type t)
  | Error e -> print_endline e

let _ = print_endline "----------------"
