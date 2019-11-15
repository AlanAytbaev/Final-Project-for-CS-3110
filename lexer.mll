{
  open Parser
  open Lexing

  exception Syntax_error 

}

let white = [' ' '\t']+
let digit = ['0'-'9']
let float = '-'? digit+ '.'? digit*
let letter = ['a'-'z' 'A'-'Z']
let bool_id = "true\\|false"
let id = letter+



rule read =
parse
|white { read lexbuf }
|"+" { ADD }
|"%" { MOD }
|"^" { EXP }
|"log" { LOG }
|"sub" { SUBT }
|"*" { MULT }
|"/" { DIV }
|"==" { EQUALS }
|"if" { IF }
|"then" { THEN }
|"else" { ELSE }
|"let" { LET }
|"=" { EQUALS }
|"in" { IN }
|"fun" {FUN}
|"->" {ARROW}
|"(" {LPAREN}
|")" {RPAREN}
|id { ID (Lexing.lexeme lexbuf) }
|float { FLT (float_of_string (Lexing.lexeme lexbuf)) }
|_ { raise (Syntax_error ) }
|eof { EOF }
