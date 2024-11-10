open CoreLib.TermTypes
open CoreLib.TypingRules
open CoreLib.LambdaUtils

let identity = Abs ("x", Var "x")
let ii = App (identity, identity)
let apply = Abs ("x", Abs ("y", App (Var "x", Var "y")))
let k_function = Abs ("x", Abs ("y", Var "x"))
let f_function = Abs ("x", Abs ("y", Var "y"))

let s_function =
  Abs
    ( "x",
      Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z"))))
    )

let skk = App (App (s_function, k_function), k_function)
let delta = Abs ("x", App (Var "x", Var "x"))
let omega = App (delta, delta)
let kiomega = App (App (k_function, identity), omega)
let xxx = Abs ("x", App (App (Var "x", Var "x"), Var "x"))
let sii = App (App (s_function, identity), identity)
let siii = App (App (App (s_function, identity), identity), identity)

(* Really tricky to write test because of how we make the variables with the counter *)
let test_identity () =
  let result = type_inference identity in
  match result with
  (* Should be of type a -> a *)
  | Ok ty ->
      let expected = "(T2 -> T2)" in
      Alcotest.(check string) "identity" expected (print_type ty)
  | Error e -> Alcotest.fail e

let test_ii () =
  let result = type_inference ii in
  match result with
  (* Should be of type a -> a *)
  | Ok ty ->
      let expected = "(T8 -> T8)" in
      Alcotest.(check string) "ii" expected (print_type ty)
  | Error e -> Alcotest.fail e

let test_apply () =
  let result = type_inference apply in
  match result with
  (* Should be of type (a -> b) -> a -> b *)
  | Ok ty ->
      let expected = "((T13 -> T14) -> (T13 -> T14))" in
      Alcotest.(check string) "apply" expected (print_type ty)
  | Error e -> Alcotest.fail e

let test_k_function () =
  let result = type_inference k_function in
  match result with
  (* Should be of type a -> b -> a *)
  | Ok ty ->
      let expected = "(T17 -> (T19 -> T17))" in
      Alcotest.(check string) "k_function" expected (print_type ty)
  | Error e -> Alcotest.fail e

let test_f_function () =
  let result = type_inference f_function in
  match result with
  (* Should be of type a -> b -> b *)
  | Ok ty ->
      let expected = "(T22 -> (T24 -> T24))" in
      Alcotest.(check string) "f_function" expected (print_type ty)
  | Error e -> Alcotest.fail e

let test_s_function () =
  let result = type_inference s_function in
  match result with
  (* Should be of type (a -> b -> c) -> (a -> b) -> a -> c *)
  | Ok ty ->
      let expected =
        "((T31 -> (T33 -> T32)) -> ((T31 -> T33) -> (T31 -> T32)))"
      in
      Alcotest.(check string) "s_function" expected (print_type ty)
  | Error e -> Alcotest.fail e

let test_skk () =
  let result = type_inference skk in
  match result with
  (* Should be of type a -> a *)
  | Ok ty ->
      let expected = "(T52 -> T52)" in
      Alcotest.(check string) "skk" expected (print_type ty)
  | Error e -> Alcotest.fail e

let test_delta () =
  let result = type_inference delta in
  match result with
  (* Should be untypable because of recursive type *)
  | Ok _ -> Alcotest.fail "delta should not be typable"
  | Error e -> Alcotest.(check string) "delta" "Type error: recursion" e

let test_omega () =
  let result = type_inference omega in
  match result with
  (* Should be untypable because of recursive type *)
  | Ok _ -> Alcotest.fail "omega should not be typable"
  | Error e -> Alcotest.(check string) "omega" "Type error: recursion" e

let test_kiomega () =
  let result = type_inference kiomega in
  match result with
  (* Should be of type a -> a because K I ω = I *)
  (* But the type inference is not smart enough to figure that out *)
  (* It tries to unify ω which is diverging *)
  | Ok _ -> Alcotest.fail "kiomega should not be typable"
  | Error e -> Alcotest.(check string) "kiomega" "Type error: recursion" e

let test_xxx () =
  let result = type_inference xxx in
  match result with
  (* Should be untypable because of recursive type *)
  | Ok _ -> Alcotest.fail "xxx should not be typable"
  | Error e -> Alcotest.(check string) "xxx" "Type error: recursion" e

let test_sii () =
  let result = type_inference sii in
  match result with
  (* Should be untypable because of recursive type *)
  | Ok _ -> Alcotest.fail "sii should not be typable"
  | Error e -> Alcotest.(check string) "sii" "Type error: recursion" e

let test_siii () =
  let result = type_inference siii in
  match result with
  (* Should be untypable because of recursive type *)
  | Ok _ -> Alcotest.fail "siii should not be typable"
  | Error e -> Alcotest.(check string) "siii" "Type error: recursion" e

let () =
  let open Alcotest in
  run "Lambda"
    [
      ( "Type inference test",
        [
          test_case "identity" `Quick test_identity;
          test_case "ii" `Quick test_ii;
          test_case "apply" `Quick test_apply;
          test_case "k_function" `Quick test_k_function;
          test_case "f_function" `Quick test_f_function;
          test_case "s_function" `Quick test_s_function;
          test_case "skk" `Quick test_skk;
          test_case "delta" `Quick test_delta;
          test_case "omega" `Quick test_omega;
          test_case "kiomega" `Quick test_kiomega;
          test_case "xxx" `Quick test_xxx;
          test_case "sii" `Quick test_sii;
          test_case "siii" `Quick test_siii;
        ] );
    ]
