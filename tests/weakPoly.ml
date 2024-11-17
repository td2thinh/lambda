open CoreLib.TermTypes
open CoreLib.TypingRules

let term = Let ("x", Ref (Val 3), App (Abs ("y", Deref (Var "x")), Val 4))
let term2 = Let ("f", Abs ("x", Var "x"), App (Var "f", Val 3))

let term3 =
  Let
    ( "r",
      Ref (Val 0),
      Let
        ( "f",
          Abs ("x", Deref (Var "r")),
          Add (App (Var "f", Val 1), App (Var "f", Val 42)) ) )

let term4 =
  Let
    ( "r",
      Ref (Val 0),
      Let
        ( "_ignored",
          Assign (Var "r", Val 42),
          Let ("f", Abs ("x", Deref (Var "r")), App (Var "f", Unit)) ) )

let term5 =
  Let
    ( "r",
      Ref (Val 0),
      Let
        ( "g",
          Abs
            ( "x",
              Let ("r2", Ref (Var "x"), Add (Deref (Var "r"), Deref (Var "r2")))
            ),
          App (Var "g", Val 1) ) )

let type_test =
  Alcotest.testable CoreLib.LambdaUtils.pp_type
    CoreLib.LambdaUtils.alpha_equal_type

let test_term_1 () =
  let expected = TNat in
  let result = type_inference term in
  match result with
  | Ok t -> Alcotest.(check type_test) "same type" expected t
  | Error e -> Alcotest.fail e

let test_term_2 () =
  let expected = TNat in
  let result = type_inference term2 in
  match result with
  | Ok t -> Alcotest.(check type_test) "same type" expected t
  | Error e -> Alcotest.fail e

let test_term_3 () =
  let expected = TNat in
  let result = type_inference term3 in
  match result with
  | Ok t -> Alcotest.(check type_test) "same type" expected t
  | Error e -> Alcotest.fail e

let test_term_4 () =
  let expected = TNat in
  let result = type_inference term4 in
  match result with
  | Ok t -> Alcotest.(check type_test) "same type" expected t
  | Error e -> Alcotest.fail e

let test_term_5 () =
  let expected = TNat in
  let result = type_inference term5 in
  match result with
  | Ok t -> Alcotest.(check type_test) "same type" expected t
  | Error e -> Alcotest.fail e

let () =
  let open Alcotest in
  run "Type Inference"
    [
      ( "Type Inference",
        [
          test_case "term 1" `Quick test_term_1;
          test_case "term 2" `Quick test_term_2;
          test_case "term 3" `Quick test_term_3;
          test_case "term 4" `Quick test_term_4;
          test_case "term 5" `Quick test_term_5;
        ] );
    ]
