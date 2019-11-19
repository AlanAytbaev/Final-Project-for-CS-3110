%{
open Ast
open List
%}

%token <bool> BOOL
%token <float> FLT
%token <string> ID
%token ADD MULT LOG MOD EXP SUBT DIV EQUALS
%token LPAREN RPAREN ARROW
%token FUN IN LET IF THEN ELSE 
%token EOF
%token SIN
%token COS
%token TAN
%token SEC
%token COSEC
%token COTAN
%token RAD
%token DEG


%nonassoc IN
%nonassoc THEN
%nonassoc ELSE
%nonassoc LET
%right ARROW
%left EQUALS
%left ADD SUBT
%left MULT DIV MOD LOG EXP


%start <Ast.phrase> prog

%%

prog:
|e = expr; EOF { Expr e }
|d = defn; EOF { Defn d }
;

expr:
|e = s_expr { e }
|e = s_expr; es = nonempty_list(s_expr) { FunApp (e, es) }
|f = FLT { Float f }
|b = BOOL { Boolean b }
|x = iden { Var x }
|e1 = expr; ADD; e2 = expr { Binop (Func "+", e1, e2) }
|e1 = expr; SUBT; e2 = expr { Binop (Func "-", e1, e2) }
|e1 = expr; MULT; e2 = expr { Binop (Func "*", e1, e2) }
|e1 = expr; DIV; e2 = expr { Binop (Func "/", e1, e2) }
|e1 = expr; MOD; e2 = expr { Binop (Func "%", e1, e2)}
|e1 = expr; LOG; e2 = expr { Binop (Func "log", e1, e2)}
|e1 = expr; EXP; e2 = expr { Binop (Func "^", e1, e2)}
|e1 = expr; EQUALS; e2 = expr { Binop (Func "==", e1, e2)}
|IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
|LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
|FUN; LPAREN; xs = nonempty_list(iden); RPAREN; ARROW; e = expr; { Fun (xs, e) }


s_expr: 
| x = iden { Var x }
| LPAREN; e = expr; RPAREN
        { e }


defn:
|LET; x = ID; EQUALS; e1 = expr { DLet (x, e1) }

iden:
  | x = ID
        { x }

