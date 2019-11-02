
(* The type of tokens. *)

type token = 
  | SUBT
  | MULT
  | FLT of (float)
  | EOF
  | DIV
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
