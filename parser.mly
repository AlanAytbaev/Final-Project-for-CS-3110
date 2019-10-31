%{ 
open Ast
%}

%token <float> FLT
%token ADD
%token MULT
%token SUBT
%token DIV
%token EOF

%left ADD
%left SUBT
%left MULT
%left DIV

%start<Ast.expr> prog

%%

prog:
|e = expr; EOF { e }
;

expr:
|f = FLT { Flt f }
|e1 = expr; ADD; e2 = expr { Add (e1, e2) }
|e1 = expr; SUBT; e2 = expr { Subt (e1, e2) }
|e1 = expr; MULT; e2 = expr { Mult (e1, e2) }
|e1 = expr; DIV; e2 = expr { Div (e1, e2) }
;

