open CoreLib.LambdaUtils
open CoreLib.LambdaRules
open CoreLib.TypingRules
open Lambda_bin.Parser
open Lambda_bin.Lexer

let option : int ref = ref (-1)

let rec loop lexbuf =
  try
    Printf.printf "> ";
    flush stdout;
    match prog read lexbuf with
    | Some t ->
        if !option = 0 then (
          match ltr_cbv_norm t with
          | Ok t' ->
              Printf.printf "%s\n" (print_term t');
              loop lexbuf
          | Error e ->
              Printf.printf "Error: %s\n" e;
              loop lexbuf)
        else if !option = 1 then (
          match type_inference t with
          | Ok ty ->
              Printf.printf "%s\n" (print_type ty);
              loop lexbuf
          | Error e ->
              Printf.printf "Error: %s\n" e;
              loop lexbuf)
        else if !option = 2 then (
          Printf.printf "%s\n" (print_term t);
          loop lexbuf)
        else Printf.printf "Invalid option\n"
    | None -> if Lexing.lexeme lexbuf = "" then () else loop lexbuf
  with
  | Error ->
      Printf.eprintf "Syntax error at position %d\n"
        (Lexing.lexeme_start lexbuf);
      Lexing.flush_input lexbuf;
      loop lexbuf
  | Failure msg ->
      Printf.eprintf "Lexing error: %s\n" msg;
      Lexing.flush_input lexbuf;
      loop lexbuf

let rec choose_option () =
  Printf.printf "Choose an option:\n";
  Printf.printf "0: Evaluate\n";
  Printf.printf "1: Type check\n";
  Printf.printf "2: Just Parse and Print\n";
  Printf.printf "> ";
  flush stdout;
  let input = read_line () in
  match input with
  | "0" -> option := 0
  | "1" -> option := 1
  | "2" -> option := 2
  | _ ->
      Printf.printf "Invalid option\n Please choose again\n";
      choose_option ()

let () =
  Printf.printf "Welcome to a little lambda calculus interpreter\n";
  choose_option ();
  if !option = 0 then Printf.printf "Evaluation mode\n"
  else if !option = 1 then Printf.printf "Type checking mode\n"
  else Printf.printf "Parse and print mode\n";
  loop (Lexing.from_channel stdin)
