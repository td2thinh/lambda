open CoreLib.TermTypes
open CoreLib.LambdaUtils

(* open CoreLib.TypingRules *)
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

(* let list_ref = ref [1; 2] *)
let list_ref_1_2 = Ref (List [ Val 1; Val 2 ])

let update_list_value =
  Let
    ( "l",
      list_ref_1_2,
      Let
        ( "_",
          Assign (Deref (Var "l"), Cons (Val 3, Cons (Val 4, Deref (Var "l")))),
          Deref (Var "l") ) )

let _ =
  match ltr_cbv_norm let_assign_x_0_plus_1 with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e

let _ =
  match ltr_cbv_norm update_list_value with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e
