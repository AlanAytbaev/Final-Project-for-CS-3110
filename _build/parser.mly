%{
open Ast
%}

%token <bool> BOOL
%token <float> FLT
%token <string> ID
%token ADD
%token MULT
%token LOG
%token MOD
%token EXP
%token SUBT
%token DIV
%token EOF
%token LET
%token IF
%token THEN
%token ELSE
%token EQUALS

%left ADD
%left SUBT
%left MULT
%left DIV
%left MOD
%left LOG
%left EXP

%start <Ast.expr> prog

%%

prog:
|e = expr; EOF { e }
;

expr:
|f = FLT { Float f }
|b = BOOL { Boolean b }
|e1 = expr; ADD; e2 = expr { Binop (Func "+", e1, e2) }
|e1 = expr; SUBT; e2 = expr { Binop (Func "-", e1, e2) }
|e1 = expr; MULT; e2 = expr { Binop (Func "*", e1, e2) }
|e1 = expr; DIV; e2 = expr { Binop (Func "/", e1, e2) }
|e1 = expr; MOD; e2 = expr { Binop (Func "%", e1, e2)}
|e1 = expr; LOG; e2 = expr { Binop (Func "log", e1, e2)}
|e1 = expr; EXP; e2 = expr { Binop (Func "^", e1, e2)}
|LET; x = ID; EQUALS; e1 = expr { Let (x, e1) }
|IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
