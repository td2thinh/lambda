open CoreLib.TermTypes
open CoreLib.LambdaUtils

(* open CoreLib.TypingRules *)
open CoreLib.LambdaRules

(* let ex_plus_4_5 =
     App (App (Abs ("x", Abs ("y", Add (Var "x", Var "y"))), Val 4), Val 5)

   let ex_minus_4_5 =
     App (App (Abs ("x", Abs ("y", Sub (Var "x", Var "y"))), Val 4), Val 5)

   let ex_list_4_5_6 = List [ Val 4; Val 5; Val 6 ]
   let ex_cons_1_2_3 = Cons (Val 1, Cons (Val 2, Val 3))
   let ex_cons_456_123 = Cons (ex_list_4_5_6, ex_cons_1_2_3)
   let ex_cons_123_456 = Cons (ex_cons_1_2_3, ex_list_4_5_6)
   let if0_4_5_6 = IfZero (Val 0, Val 5, Val 6)
   let ifempty_4_5_6 = IfEmpty (ex_list_4_5_6, Val 5, Val 6)

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

   let _ =
     match ltr_cbv_norm ex_plus_4_5 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm ex_minus_4_5 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm ex_list_4_5_6 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm ex_cons_1_2_3 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm if0_4_5_6 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm ifempty_4_5_6 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm test_sum with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm ex_mult_4_5 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm ex_factoriel_5 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm ex_map_plus_1 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm test_head with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm test_tail with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm test_head_empty with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm test_tail_empty with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm ex_cons_456_123 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm ex_cons_123_456 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm let_x1_x2_plus_4_5 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e *)
(* let list_1_2_3_4_5_6_7 =
     List [ Val 1; Val 2; Val 3; Val 4; Val 5; Val 6; Val 7 ]

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

   let map_add_one = App (map_lambda_rec, Abs ("x", Add (Var "x", Val 1)))
   let ex_map_plus_1 = App (map_add_one, list_1_2_3_4_5_6_7)

   let let_map_test =
     Let
       ( "map2",
         map_lambda_rec,
         App (App (Var "map2", Abs ("x", Add (Var "x", Val 1))), list_1_2_3_4_5_6_7)
       )

   let _ =
     match ltr_cbv_norm ex_map_plus_1 with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e

   let _ =
     match ltr_cbv_norm let_map_test with
     | Ok t -> print_endline (print_term t)
     | Error e -> print_endline e *)
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

let add_one = Abs ("x", Add (Var "x", Val 1))

let ex_map_plus_1 =
  App (App (map_lambda_rec, add_one), List [ Val 1; Val 2; Val 3; Val 4; Val 5 ])

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
  App (sum_using_fold_right, List [ Val 1; Val 2; Val 3; Val 4; Val 5 ])

let test_cons_list =
  Cons (Cons (Val 1, Val 2), Cons (Cons (Val 3, Val 4), List []))

let _ =
  match ltr_cbv_norm ex_map_plus_1 with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e

let _ =
  match ltr_cbv_norm ex_fold_right with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e

let _ =
  match ltr_cbv_norm test_cons_list with
  | Ok t -> print_endline (print_term t)
  | Error e -> print_endline e
