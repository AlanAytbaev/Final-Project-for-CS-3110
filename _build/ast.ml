type bop = 
  |Add
  |Subt
  |Div
  |Mult

type expr = 
  |Float of float
  |Binop of bop * expr * expr
