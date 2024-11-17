open CoreLib.TermTypes
open CoreLib.LambdaUtils
open CoreLib.TypingRules
(* open CoreLib.LambdaRules *)

(* let x = ref 0 in x := (x + 1) in x *)
let let_assign_x_0_plus_1 =
  Let
    ( "x",
      Ref (Val 0),
      Let
        ( "_",
          Assign (Deref (Var "x"), Add (Deref (Var "x"), Val 1)),
          Deref (Var "x") ) )

let list_ref_1_2 = Ref (List [ Val 1; Val 2 ])

let update_list_value =
  Let
    ( "l",
      list_ref_1_2,
      Let
        ( "_",
          Assign (Var "l", Cons (Val 3, Cons (Val 4, Deref (Var "l")))),
          Deref (Var "l") ) )

let make_number_list_function =
  Fix
    (Abs
       ( "make_number_list",
         Abs
           ( "n",
             IfZero
               ( Var "n",
                 List [],
                 Cons
                   (Var "n", App (Var "make_number_list", Sub (Var "n", Val 1)))
               ) ) ))

let sum_list =
  Fix
    (Abs
       ( "sum_list",
         Abs
           ( "l",
             IfEmpty
               ( Var "l",
                 Val 0,
                 Add (Head (Var "l"), App (Var "sum_list", Tail (Var "l"))) ) )
       ))

let sum_all_numbers_in_list =
  Let
    ( "make_number_list",
      make_number_list_function,
      Let
        ( "sum_list",
          sum_list,
          Let
            ( "list_1_2_3_4_5_6_7",
              App (Var "make_number_list", Val 7),
              App (Var "sum_list", Var "list_1_2_3_4_5_6_7") ) ) )

let _ = print_endline (print_term sum_all_numbers_in_list)

let _ =
  match type_inference let_assign_x_0_plus_1 with
  | Ok t -> print_endline (print_type t)
  | Error e -> print_endline e

let _ =
  match type_inference update_list_value with
  | Ok t -> print_endline (print_type t)
  | Error e -> print_endline e
