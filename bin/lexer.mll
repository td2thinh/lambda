{
open Lexing
open Parser
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let number = digit+
let string = [^ '(' ')' '\\' '.' '#' ' ' '\t' '\n' '\r' '+' '-' '*' '=' '[' ']' ':' '!' ';' ]+

rule read =
  parse
    | white    { read lexbuf }
    | "\\"     { LAMBDA }
    | "λ"      { LAMBDA }
    | "."      { DOT }
    | "("      { LPAREN }
    | ")"      { RPAREN }
    | "["      { LBRACK }
    | "]"      { RBRACK }
    | ";"      { SEMICOLON }
    | "let"    { LET }
    | "in"     { IN }
    | "="      { EQ }
    | "+"      { PLUS }
    | "-"      { MINUS }
    | "*"      { MULT }
    | "fix"    { FIX }
    | "if"     { IF }
    | "then"   { THEN }
    | "else"   { ELSE }
    | "ifzero" { IFZERO }
    | "ifempty" { IFEMPTY }
    | "::"     { CONS }
    | "[]"     { NIL }
    | "head"   { HEAD }
    | "tail"   { TAIL }
    | "unit"   { UNIT }
    | "ref"    { REF }
    | "!"      { DEREF }
    | ":="     { ASSIGN }
    | number   { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "#"      { skip_line lexbuf }
    | string   { ID (Lexing.lexeme lexbuf) }
    | newline  { new_line lexbuf; END }
    | eof      { EOF }

and skip_line =
  parse
    | newline  { new_line lexbuf; read lexbuf }
    | eof      { EOF }
    | _        { skip_line lexbuf }