type bop =
  |Add
  |Mult
  |Subt
  |Div


type expr =
  |Float of float
  |Binop of bop * expr * expr
