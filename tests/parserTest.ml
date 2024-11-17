open CoreLib.TermTypes
open CoreLib.LambdaRules
open Lambda_bin.Parser
open Lambda_bin.Lexer

(* Helper functions *)
let parse_string input =
  let lexbuf = Lexing.from_string input in
  prog read lexbuf

let term =
  Alcotest.testable CoreLib.LambdaUtils.pp CoreLib.LambdaUtils.alpha_equal

let test_parse_eval input expected_parse expected_eval () =
  match parse_string input with
  | Some parsed_term -> (
      Alcotest.(check term) "parsed term" expected_parse parsed_term;
      match ltr_cbv_norm parsed_term with
      | Ok evaluated_term ->
          Alcotest.(check term) "evaluated term" expected_eval evaluated_term
      | Error e -> Alcotest.fail e)
  | None -> Alcotest.fail ("Failed to parse: " ^ input)

let test_parse input expected () =
  match parse_string input with
  | Some parsed_term -> Alcotest.(check term) "parsed term" expected parsed_term
  | None -> Alcotest.fail ("Failed to parse: " ^ input)

(* Test cases *)
let basic_tests =
  [
    ("identity", `Quick, test_parse "\\x.x" (Abs ("x", Var "x")));
    ( "application",
      `Quick,
      test_parse "(\\x.x) y" (App (Abs ("x", Var "x"), Var "y")) );
    ( "nested abstraction",
      `Quick,
      test_parse "\\x.\\y.x" (Abs ("x", Abs ("y", Var "x"))) );
  ]

let arithmetic_tests =
  [
    ( "simple addition",
      `Quick,
      test_parse_eval "1 + 2" (Add (Val 1, Val 2)) (Val 3) );
    ( "multiplication",
      `Quick,
      test_parse_eval "3 * 4" (Mult (Val 3, Val 4)) (Val 12) );
    ( "complex arithmetic",
      `Quick,
      test_parse_eval "let x = 5 in x + 3"
        (Let ("x", Val 5, Add (Var "x", Val 3)))
        (Val 8) );
  ]

let list_tests =
  [
    ("empty list", `Quick, test_parse_eval "[]" (List []) (List []));
    ( "simple list",
      `Quick,
      test_parse_eval "[1; 2; 3]"
        (List [ Val 1; Val 2; Val 3 ])
        (List [ Val 1; Val 2; Val 3 ]) );
    ( "cons operation",
      `Quick,
      test_parse_eval "1 :: [2; 3]"
        (Cons (Val 1, List [ Val 2; Val 3 ]))
        (List [ Val 1; Val 2; Val 3 ]) );
    ( "head operation",
      `Quick,
      test_parse_eval "head [1; 2; 3]"
        (Head (List [ Val 1; Val 2; Val 3 ]))
        (Val 1) );
  ]

let conditional_tests =
  [
    ( "ifzero true case",
      `Quick,
      test_parse_eval "ifzero 0 then 1 else 2"
        (IfZero (Val 0, Val 1, Val 2))
        (Val 1) );
    ( "ifzero false case",
      `Quick,
      test_parse_eval "ifzero 1 then 1 else 2"
        (IfZero (Val 1, Val 1, Val 2))
        (Val 2) );
    ( "ifempty true case",
      `Quick,
      test_parse_eval "ifempty [] then 1 else 2"
        (IfEmpty (List [], Val 1, Val 2))
        (Val 1) );
  ]

let reference_tests =
  [
    ("reference", `Quick, test_parse_eval "ref 5" (Ref (Val 5)) (Region 1));
    ( "dereference",
      `Quick,
      test_parse_eval "let r = ref 5 in !r"
        (Let ("r", Ref (Val 5), Deref (Var "r")))
        (Val 5) );
  ]

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

(* let ex_map_plus_1 =
   App
     ( App (map_lambda_rec, Abs ("x", Add (Var "x", Val 1))),
       List [ Val 4; Val 5; Val 6 ] ) *)

let map_tests =
  [
    ( "map function",
      `Quick,
      test_parse_eval
        "let map = fix (\\map.\\f.\\l.ifempty l [] (f (head l) (map f (tail \
         l))))"
        map_lambda_rec map_lambda_rec );
  ]

(* Run the tests *)
let () =
  Alcotest.run "Lambda Calculus Parser Tests"
    [
      ("basic lambda expressions", basic_tests);
      ("arithmetic operations", arithmetic_tests);
      ("list operations", list_tests);
      ("conditional expressions", conditional_tests);
      ("reference operations", reference_tests);
      ("map function", map_tests);
    ]
