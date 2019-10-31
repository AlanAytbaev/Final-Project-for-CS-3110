%{ 
open Ast
%}

%token <float> FLOAT
%token ADD
%token MULT
%token SUBT
%token DIV
%token EOF

%left ADD
%left MULT
%left SUBT
%left DIV

%start prog

%%

prog:
|e = expr; EOF { e }
;

expr:
|f = FLOAT { Float f }
|e1 = expr; ADD; e2 = expr { Binop (Add, e1, e2) }
|e1 = expr; MULT; e2 = expr { Binop (Mult, e1, e2) }
|e1 = expr; SUBT; e2 = expr { Binop (Subt, e1, e2) }
|e1 = expr; DIV; e2 = expr { Binop (Div, e1, e2) }
;

