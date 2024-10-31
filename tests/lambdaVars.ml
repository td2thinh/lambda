open CoreLib.LambdaUtils
open CoreLib.TermTypes

(* Testing free and bound variables *)
let example =
  Abs
    ( "x",
      (* λx. *)
      App
        ( Abs
            ("y", (* λy. *)
                  Abs ("x", (* λx. *)
                            App (Var "y", Var "x") (* y x *))),
          App (Var "x", Var "y") (* (x y) *) ) )

let test_variables () =
  let result = variables example in
  Alcotest.(check (list string)) "variables" [ "x"; "y" ] result

let test_free_vars () =
  let result = free_vars example in
  Alcotest.(check (list string)) "free_vars" [ "y" ] result

let test_bound_vars () =
  let result = bound_vars example in
  Alcotest.(check (list string)) "bound_vars" [ "x" ] result

let () =
  let open Alcotest in
  run "Lambda"
    [
      ( "LambdaUtils",
        [
          test_case "variables" `Quick test_variables;
          test_case "free_vars" `Quick test_free_vars;
          test_case "bound_vars" `Quick test_bound_vars;
        ] );
    ]
