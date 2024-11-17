open CoreLib.TermTypes
open CoreLib.LambdaUtils
open CoreLib.TypingRules
(* open CoreLib.LambdaRules *)

let let_assign_x_0_plus_1 =
  Let
    ( "x",
      Ref (Val 0),
      Let ("_", Assign (Var "x", Add (Deref (Var "x"), Val 1)), Deref (Var "x"))
    )

let list_ref_1_2 = Ref (List [ Val 1; Val 2 ])

let update_list_value =
  Let
    ( "l",
      list_ref_1_2,
      Let
        ( "_",
          Assign (Var "l", Cons (Val 3, Cons (Val 4, Deref (Var "l")))),
          Deref (Var "l") ) )

let _ =
  match type_inference let_assign_x_0_plus_1 with
  | Ok t -> print_endline (print_type t)
  | Error e -> print_endline e

let _ =
  match type_inference update_list_value with
  | Ok t -> print_endline (print_type t)
  | Error e -> print_endline e
