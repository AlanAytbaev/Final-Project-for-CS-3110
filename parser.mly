%{
open Ast
%}

%token <float> FLT
%token <string> ID
%token ADD
%token MULT
%token SUBT
%token DIV
%token EOF
%token LET
%token EQUALS
%token IN

%nonassoc IN
%left ADD
%left SUBT
%left MULT
%left DIV

%start <Ast.expr> prog

%%

prog:
|e = expr; EOF { e }
;

expr:
|f = FLT { Float f }
|x = ID { Var x }
|e1 = expr; ADD; e2 = expr { Binop (Func "+", e1, e2) }
|e1 = expr; SUBT; e2 = expr { Binop (Func "-", e1, e2) }
|e1 = expr; MULT; e2 = expr { Binop (Func "*", e1, e2) }
|e1 = expr; DIV; e2 = expr { Binop (Func "/", e1, e2) }
|LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }

