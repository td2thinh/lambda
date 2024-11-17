open CoreLib.TermTypes
open CoreLib.LambdaRules

let term_test =
  Alcotest.testable CoreLib.LambdaUtils.pp CoreLib.LambdaUtils.alpha_equal

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

let counter_fun =
  Fix
    (Abs
       ( "counter",
         Let
           ( "counter",
             Ref (Val 0),
             Let
               ( "_",
                 Assign
                   (Deref (Var "counter"), Add (Deref (Var "counter"), Val 1)),
                 Deref (Var "counter") ) ) ))

let counter_plus_10 = Add (Add (counter_fun, counter_fun), Val 8)

let test_counter_plus_10 () =
  let expected = Val 10 in
  let result = ltr_cbv_norm counter_plus_10 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_assign_x_0_plus_1 () =
  let expected = Val 1 in
  let result = ltr_cbv_norm let_assign_x_0_plus_1 in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let test_update_list_value () =
  let expected = List [ Val 3; Val 4; Val 1; Val 2 ] in
  let result = ltr_cbv_norm update_list_value in
  match result with
  | Ok t -> Alcotest.(check term_test) "same term" expected t
  | Error e -> Alcotest.fail e

let () =
  let open Alcotest in
  run "Lambda"
    [
      ( "Assign and Ref test",
        [
          test_case "assign_x_0_plus_1" `Quick test_assign_x_0_plus_1;
          test_case "update_list_value" `Quick test_update_list_value;
          test_case "let_counter" `Quick test_counter_plus_10;
        ] );
    ]
