open CoreLib.TermTypes
open CoreLib.TypingRules

let ex_plus_4_5 =
  App (App (Abs ("x", Abs ("y", Add (Var "x", Var "y"))), Val 4), Val 5)

let ex_minus_4_5 =
  App (App (Abs ("x", Abs ("y", Sub (Var "x", Var "y"))), Val 4), Val 5)

let ex_list_4_5_6 = List [ Val 4; Val 5; Val 6 ]
let ex_cons_1_2_3 = Cons (Val 1, Cons (Val 2, Cons (Val 3, List [])))

let factoriel_rec =
  Fix
    (Abs
       ( "factoriel",
         Abs
           ( "n",
             IfZero
               ( Var "n",
                 Val 1,
                 Mult (Var "n", App (Var "factoriel", Sub (Var "n", Val 1))) )
           ) ))

let ex_factoriel_5 = App (factoriel_rec, Val 5)

let map_lambda_rec =
  Fix
    (Abs
       ( "map",
         Abs
           ( "f",
             Abs
               ( "l",
                 IfEmpty
                   ( Var "l",
                     List [],
                     Cons
                       ( App (Var "f", Head (Var "l")),
                         App (App (Var "map", Var "f"), Tail (Var "l")) ) ) ) )
       ))

let ex_map_plus_1 =
  App (App (map_lambda_rec, Abs ("x", Add (Var "x", Val 1))), ex_list_4_5_6)

let test_head = Head ex_list_4_5_6
let test_tail = Tail ex_list_4_5_6

let let_map_test =
  Let
    ( "map2",
      map_lambda_rec,
      App (App (Var "map2", Abs ("x", Add (Var "x", Val 1))), ex_list_4_5_6) )

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

let type_test =
  Alcotest.testable CoreLib.LambdaUtils.pp_type
    CoreLib.LambdaUtils.alpha_equal_type

let test_ex_plus_4_5 () =
  let expected = TNat in
  let result = type_inference ex_plus_4_5 in
  match result with
  | Ok ty -> Alcotest.(check type_test) "ex_plus_4_5" expected ty
  | Error e -> Alcotest.fail e

let test_ex_minus_4_5 () =
  let expected = TNat in
  let result = type_inference ex_minus_4_5 in
  match result with
  | Ok ty -> Alcotest.(check type_test) "ex_minus_4_5" expected ty
  | Error e -> Alcotest.fail e

let test_factoriel () =
  let expected = TArrow (TNat, TNat) in
  let result = type_inference factoriel_rec in
  match result with
  | Ok ty -> Alcotest.(check type_test) "factoriel" expected ty
  | Error e -> Alcotest.fail e

let test_ex_factoriel_5 () =
  let expected = TNat in
  let result = type_inference ex_factoriel_5 in
  match result with
  | Ok ty -> Alcotest.(check type_test) "ex_factoriel_5" expected ty
  | Error e -> Alcotest.fail e

let test_ex_map_plus_1 () =
  let expected = TList TNat in
  let result = type_inference ex_map_plus_1 in
  match result with
  | Ok ty -> Alcotest.(check type_test) "ex_map_plus_1" expected ty
  | Error e -> Alcotest.fail e

let test_test_head () =
  let expected = TNat in
  let result = type_inference test_head in
  match result with
  | Ok ty -> Alcotest.(check type_test) "test_head" expected ty
  | Error e -> Alcotest.fail e

let test_test_tail () =
  let expected = TList TNat in
  let result = type_inference test_tail in
  match result with
  | Ok ty -> Alcotest.(check type_test) "test_tail" expected ty
  | Error e -> Alcotest.fail e

let test_map_lambda_rec () =
  let expected =
    TArrow
      ( TArrow (TVar "T61", TVar "T62"),
        TArrow (TList (TVar "T61"), TList (TVar "T62")) )
  in
  let result = type_inference map_lambda_rec in
  match result with
  | Ok ty -> Alcotest.(check type_test) "map_lambda_rec" expected ty
  | Error e -> Alcotest.fail e

let test_let_map_test () =
  let expected = TList TNat in
  let result = type_inference let_map_test in
  match result with
  | Ok ty -> Alcotest.(check type_test) "let_map_test" expected ty
  | Error e -> Alcotest.fail e

let test_make_list () =
  let expected = TArrow (TNat, TList TNat) in
  let result = type_inference make_number_list_function in
  match result with
  | Ok ty -> Alcotest.(check type_test) "make_number_list_function" expected ty
  | Error e -> Alcotest.fail e

let test_sum_list () =
  let expected = TArrow (TList TNat, TNat) in
  let result = type_inference sum_list in
  match result with
  | Ok ty -> Alcotest.(check type_test) "sum_list" expected ty
  | Error e -> Alcotest.fail e

let test_foldr () =
  let expected =
    (* ((T122 -> (T118 -> T118)) -> (T118 -> ([T122] -> T118))) *)
    TArrow
      ( TArrow (TVar "T122", TArrow (TVar "T118", TVar "T118")),
        TArrow (TVar "T118", TArrow (TList (TVar "T122"), TVar "T118")) )
  in
  let result = type_inference foldr in
  match result with
  | Ok ty -> Alcotest.(check type_test) "foldr" expected ty
  | Error e -> Alcotest.fail e

let test_cons () =
  let expected = TList TNat in
  let result = type_inference ex_cons_1_2_3 in
  match result with
  | Ok ty -> Alcotest.(check type_test) "ex_cons_1_2_3" expected ty
  | Error e -> Alcotest.fail e

let test_sum_all_numbers_in_list () =
  let expected = TNat in
  let result = type_inference sum_all_numbers_in_list in
  match result with
  | Ok ty -> Alcotest.(check type_test) "sum_all_numbers_in_list" expected ty
  | Error e -> Alcotest.fail e

let test_let_assign_x_0_plus_1 () =
  let expected = TNat in
  let result = type_inference let_assign_x_0_plus_1 in
  match result with
  | Ok ty -> Alcotest.(check type_test) "let_assign_x_0_plus_1" expected ty
  | Error e -> Alcotest.fail e

let test_update_list_value () =
  let expected = TList TNat in
  let result = type_inference update_list_value in
  match result with
  | Ok ty -> Alcotest.(check type_test) "update_list_value" expected ty
  | Error e -> Alcotest.fail e

let () =
  let open Alcotest in
  run "Lambda"
    [
      ( "Inference tests 2",
        [
          test_case "ex_plus_4_5" `Quick test_ex_plus_4_5;
          test_case "ex_minus_4_5" `Quick test_ex_minus_4_5;
          test_case "factoriel" `Quick test_factoriel;
          test_case "ex_factoriel_5" `Quick test_ex_factoriel_5;
          test_case "ex_map_plus_1" `Quick test_ex_map_plus_1;
          test_case "test_head" `Quick test_test_head;
          test_case "test_tail" `Quick test_test_tail;
          test_case "map_lambda_rec" `Quick test_map_lambda_rec;
          test_case "let_map_test" `Quick test_let_map_test;
          test_case "sum_all_numbers_in_list" `Quick test_sum_list;
          test_case "foldr" `Quick test_foldr;
          test_case "ex_cons_1_2_3" `Quick test_cons;
          test_case "make_number_list_function" `Quick test_make_list;
          test_case "sum_all_numbers_in_list" `Quick
            test_sum_all_numbers_in_list;
          test_case "let_assign_x_0_plus_1" `Quick test_let_assign_x_0_plus_1;
          test_case "update_list_value" `Quick test_update_list_value;
        ] );
    ]
