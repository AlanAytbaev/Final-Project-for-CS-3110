type bop =
  |Add
  |Mult
  |Subt
  |Div
  |Func of string


type expr =
  |Float of float
  |Binop of bop * expr * expr
