open TermTypes

let print_term (term : lambda_term) : string =
  let rec aux (term : lambda_term) : string =
    match term with
    | Var x -> x
    | Abs (x, t) -> Printf.sprintf "(λ%s.%s)" x (aux t)
    | App (t1, t2) -> Printf.sprintf "(%s %s)" (aux t1) (aux t2)
  in
  aux term
