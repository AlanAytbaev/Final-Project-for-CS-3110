{
  open Parser
}

let white = [' ''\t']+
let digit = ['0'-'9']
let float = '-'?digit+

rule read = 
  parse 
    | white { read lexbuf }
    | "+" { ADD } 
    | "-" { SUBT }
    | "*" { MULT }
    | "/" { DIV }
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | eof { EOF }

