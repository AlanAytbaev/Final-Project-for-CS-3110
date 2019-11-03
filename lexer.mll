{
  open Parser
  open Lexing
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let float = '-'? digit+ '.'? digit*
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
parse
|white { read lexbuf }
|"+" { ADD }
|"sub" { SUBT }
|"*" { MULT }
|"/" { DIV }
| "let" { LET }
| "=" { EQUALS }
| id { ID (Lexing.lexeme lexbuf) }
|float { FLT (float_of_string (Lexing.lexeme lexbuf)) }
|eof { EOF }
