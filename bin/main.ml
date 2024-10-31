open CoreLib.TermTypes
open CoreLib.LambdaUtils
(* let counter_var : int ref = ref 0

   let fresh_var () : string =
     counter_var := !counter_var + 1;
     "X" ^ string_of_int !counter_var *)

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

let _ = print_endline (print_term example)
