open CoreLib.TermTypes
open CoreLib.LambdaRules
open CoreLib.LambdaUtils

let ex_plus_4_5 =
  App (App (Abs ("x", Abs ("y", Add (Var "x", Var "y"))), Val 4), Val 5)

let ex_minus_4_5 =
  App (App (Abs ("x", Abs ("y", Sub (Var "x", Var "y"))), Val 4), Val 5)

let ex_list_4_5_6 = List [ Val 4; Val 5; Val 6 ]
let ex_cons_1_2_3 = Cons (Val 1, Cons (Val 2, Cons (Val 3, List [])))
let ex_cons_456_123 = Cons (ex_list_4_5_6, ex_cons_1_2_3)
let ex_cons_123_456 = Cons (ex_cons_1_2_3, ex_list_4_5_6)
let if0_4_5_6 = IfZero (Val 4, Val 5, Val 6)
let if0_0_5_6 = IfZero (Val 0, Val 5, Val 6)
let ifempty_4_5_6 = IfEmpty (ex_list_4_5_6, Val 5, Val 6)
let ifempty_4_5_6_empty = IfEmpty (List [], Val 5, Val 6)

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

let ex_mult_4_5 =
  App (App (Abs ("x", Abs ("y", Mult (Var "x", Var "y"))), Val 4), Val 5)

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
let test_head_empty = Head (List [])
let test_tail_empty = Tail (List [])

let let_x1_x2_plus_4_5 =
  Let ("x", Val 1, Let ("y", Val 2, Add (Var "x", Var "y")))

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

let term_test =
  Alcotest.testable CoreLib.LambdaUtils.pp CoreLib.LambdaUtils.alpha_equal

let test_ex_plus_4_5 () =
  let expected = Val 9 in
  let result = ltr_cbv_norm ex_plus_4_5 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_ex_minus_4_5 () =
  let expected = Val (-1) in
  let result = ltr_cbv_norm ex_minus_4_5 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_ex_list_4_5_6 () =
  let result = print_term ex_list_4_5_6 in
  let expected = "[4; 5; 6]" in
  Alcotest.(check string) "same term" expected result

let test_cons_1_2_3 () =
  let result = print_term ex_cons_1_2_3 in
  let expected = "(1 :: (2 :: (3 :: [])))" in
  Alcotest.(check string) "same term" expected result

let test_cons_1_2_3_eval () =
  let result = ltr_cbv_norm ex_cons_1_2_3 in
  let expected = "[1; 2; 3]" in
  match result with
  | Ok t -> Alcotest.(check string) "same term" expected (print_term t)
  | Error e -> Alcotest.fail e

let test_ex_cons_456_123 () =
  let result = ltr_cbv_norm ex_cons_456_123 in
  let expected = "[4; 5; 6; 1; 2; 3]" in
  match result with
  | Ok t -> Alcotest.(check string) "same term" expected (print_term t)
  | Error e -> Alcotest.fail e

let test_ex_cons_123_456 () =
  let result = ltr_cbv_norm ex_cons_123_456 in
  let expected = "[1; 2; 3; 4; 5; 6]" in
  match result with
  | Ok t -> Alcotest.(check string) "same term" expected (print_term t)
  | Error e -> Alcotest.fail e

let test_if0_4_5_6 () =
  let result = ltr_cbv_norm if0_4_5_6 in
  let expected = "6" in
  match result with
  | Ok t -> Alcotest.(check string) "same term" expected (print_term t)
  | Error e -> Alcotest.fail e

let test_if0_0_5_6 () =
  let result = ltr_cbv_norm if0_0_5_6 in
  let expected = "5" in
  match result with
  | Ok t -> Alcotest.(check string) "same term" expected (print_term t)
  | Error e -> Alcotest.fail e

let test_ifempty_4_5_6 () =
  let result = ltr_cbv_norm ifempty_4_5_6 in
  let expected = Val 6 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_ifempty_4_5_6_empty () =
  let result = ltr_cbv_norm ifempty_4_5_6_empty in
  let expected = Val 5 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_sum () =
  let result = ltr_cbv_norm test_sum in
  let expected = Val 55 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_ex_mult_4_5 () =
  let result = ltr_cbv_norm ex_mult_4_5 in
  let expected = Val 20 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_ex_factoriel_5 () =
  let result = ltr_cbv_norm ex_factoriel_5 in
  let expected = Val 120 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_ex_map_plus_1 () =
  let result = ltr_cbv_norm ex_map_plus_1 in
  let expected = "[5; 6; 7]" in
  match result with
  | Ok t -> Alcotest.(check string) "same term" expected (print_term t)
  | Error e -> Alcotest.fail e

let test_head () =
  let result = ltr_cbv_norm test_head in
  let expected = Val 4 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_tail () =
  let result = ltr_cbv_norm test_tail in
  let expected = "[5; 6]" in
  match result with
  | Ok t -> Alcotest.(check string) "same term" expected (print_term t)
  | Error e -> Alcotest.fail e

let test_head_empty () =
  let result = ltr_cbv_norm test_head_empty in
  let expected = test_head_empty in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_tail_empty () =
  let result = ltr_cbv_norm test_tail_empty in
  let expected = test_tail_empty in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_let_x1_x2_plus_4_5 () =
  let result = ltr_cbv_norm let_x1_x2_plus_4_5 in
  let expected = Val 3 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_let_map () =
  let result = ltr_cbv_norm let_map_test in
  let expected = "[5; 6; 7]" in
  match result with
  | Ok t -> Alcotest.(check string) "same term" expected (print_term t)
  | Error e -> Alcotest.fail e

let test_make_then_sum_list () =
  let result = ltr_cbv_norm sum_all_numbers_in_list in
  let expected = Val 28 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_sum_using_foldr () =
  let result = ltr_cbv_norm ex_fold_right in
  let expected = Val 15 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let () =
  let open Alcotest in
  run "Lambda"
    [
      ( "PCF tests",
        [
          test_case "ex_plus_4_5" `Quick test_ex_plus_4_5;
          test_case "ex_minus_4_5" `Quick test_ex_minus_4_5;
          test_case "ex_list_4_5_6" `Quick test_ex_list_4_5_6;
          test_case "cons_1_2_3" `Quick test_cons_1_2_3;
          test_case "cons_1_2_3_eval" `Quick test_cons_1_2_3_eval;
          test_case "ex_cons_456_123" `Quick test_ex_cons_456_123;
          test_case "ex_cons_123_456" `Quick test_ex_cons_123_456;
          test_case "if0_4_5_6" `Quick test_if0_4_5_6;
          test_case "if0_0_5_6" `Quick test_if0_0_5_6;
          test_case "ifempty_4_5_6" `Quick test_ifempty_4_5_6;
          test_case "ifempty_4_5_6_empty" `Quick test_ifempty_4_5_6_empty;
          test_case "sum" `Quick test_sum;
          test_case "ex_mult_4_5" `Quick test_ex_mult_4_5;
          test_case "ex_factoriel_5" `Quick test_ex_factoriel_5;
          test_case "ex_map_plus_1" `Quick test_ex_map_plus_1;
          test_case "head" `Quick test_head;
          test_case "tail" `Quick test_tail;
          test_case "head_empty" `Quick test_head_empty;
          test_case "tail_empty" `Quick test_tail_empty;
          test_case "let_x1_x2_plus_4_5" `Quick test_let_x1_x2_plus_4_5;
          test_case "let_map" `Quick test_let_map;
          test_case "make_then_sum_list" `Quick test_make_then_sum_list;
          test_case "sum_using_foldr" `Quick test_sum_using_foldr;
        ] );
    ]
