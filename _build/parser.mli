
(* The type of tokens. *)

type token = 
  | THEN
  | SUBT
  | MULT
  | LET
  | IF
  | ID of (string)
  | FLT of (float)
  | EQUALS
  | EOF
  | ELSE
  | DIV
  | BOOL of (bool)
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
