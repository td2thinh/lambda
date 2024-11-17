%{
open CoreLib.TermTypes
%}
%right LAMBDA    
%right IN
%left PLUS MINUS
%left CONS      
%left APP      
%nonassoc FIX  
%token <string> ID
%token <int> INT
%token LPAREN "("
%token RPAREN ")"
%token LAMBDA "λ" BACKSLASH "\\" 
%token DOT "."
%token LET "let"
%token IN "in"
%token EQ "="
%token PLUS "+"
%token MINUS "-"
%token MULT "*"
%token FIX "fix"
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token IFZERO "ifzero"
%token IFEMPTY "ifempty"
%token CONS "::"
%token NIL "[]"
%token LBRACK "["
%token RBRACK "]"
%token SEMICOLON ";"
%token HEAD "head"
%token TAIL "tail"
%token UNIT "unit"
%token REF "ref"
%token DEREF "!"
%token ASSIGN ":="
%token END
%token EOF

%start <CoreLib.TermTypes.lambda_term option> prog
%%

let prog :=
  | EOF; { None }
  | END; p = prog; { p }
  | t = term; line_end; { Some t }

let line_end := END | EOF

let variable :=
  | x = ID; { Var x }

let element :=
  | variable
  | n = INT; { Val n }
  | "unit"; { Unit }
  | "[]"; { List [] }
  | "["; elems = separated_list(";", term); "]"; { List elems }
  | "("; x = term; ")"; { x }

let application :=
  | element
  | "fix"; t = term; { Fix t }  
  | t = application; u = element; { App (t, u) } %prec APP
  | "head"; t = element; { Head t }
  | "tail"; t = element; { Tail t }
  | "ref"; t = element; { Ref t }
  | "!"; t = element; { Deref t }

let abstraction :=
  | lambda_symbol; x = ID; "."; u = term; { Abs (x, u) }
  | lambda_symbol; x = ID; xs = nonempty_list(ID); "."; u = term; 
    { List.fold_right (fun x acc -> Abs (x, acc)) (x::xs) u }

let lambda_symbol :=
  | "λ" | "\\"  (* Accept both lambda symbol and backslash *)

let let_expr :=
  | "let"; x = ID; "="; t = term; "in"; u = term; { Let (x, t, u) }

let arithmetic :=
  | t = term; "+"; u = term; { Add (t, u) }
  | t = term; "-"; u = term; { Sub (t, u) }
  | t = term; "*"; u = term; { Mult (t, u) }

let list_expr :=
  | t = term; "::"; u = term; { Cons (t, u) } %prec CONS

let conditional :=
  | "ifzero"; cond = term; "then"; t = term; "else"; e = term;
    { IfZero (cond, t, e) }
  | "ifempty"; cond = term; "then"; t = term; "else"; e = term;  
    { IfEmpty (cond, t, e) }

let assignment :=
  | t = term; ":="; u = term; { Assign (t, u) }

let term :=
  | element
  | application    
  | list_expr   
  | abstraction   
  | let_expr
  | arithmetic 
  | conditional
  | assignment
