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
%token IN
%token SIN
%token COS
%token TAN
%token SEC
%token COSEC
%token COTAN
%token RAD
%token DEG

%nonassoc IN
%left ADD
%left SUBT
%left MULT
%left DIV
%left MOD
%left LOG
%left EXP
%left EQUALS

%start <Ast.phrase> prog

%%

prog:
|e = expr; EOF { Expr e }
|d = defn; EOF { Defn d }
;

expr:
|f = FLT { Float f }
|b = BOOL { Boolean b }
|x = ID { Var x }
|e1 = expr; ADD; e2 = expr { Binop (Func "+", e1, e2) }
|e1 = expr; SUBT; e2 = expr { Binop (Func "-", e1, e2) }
|e1 = expr; MULT; e2 = expr { Binop (Func "*", e1, e2) }
|e1 = expr; DIV; e2 = expr { Binop (Func "/", e1, e2) }
|e1 = expr; MOD; e2 = expr { Binop (Func "%", e1, e2)}
|e1 = expr; LOG; e2 = expr { Binop (Func "log", e1, e2)}
|e1 = expr; EXP; e2 = expr { Binop (Func "^", e1, e2)}
|e1 = expr; EQUALS; e2 = expr { Binop (Func "==", e1, e2)}
|e1 = expr; SIN; e2 = expr { Binop (Func "sin", e1, e2)}
|e1 = expr; COS; e2 = expr { Binop (Func "cos", e1, e2)}
|e1 = expr; TAN; e2 = expr { Binop (Func "tan", e1, e2)}
|e1 = expr; SEC; e2 = expr { Binop (Func "sec", e1, e2)}
|e1 = expr; COSEC; e2 = expr { Binop (Func "cosec", e1, e2)}
|e1 = expr; COTAN; e2 = expr { Binop (Func "cotan", e1, e2)}
|e1 = expr; RAD; e2 = expr { Binop (Func "rad", e1, e2)}
|e1 = expr; DEG; e2 = expr { Binop (Func "deg", e1, e2)}
|IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
|LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }

defn:
|LET; x = ID; EQUALS; e1 = expr { DLet (x, e1) }

