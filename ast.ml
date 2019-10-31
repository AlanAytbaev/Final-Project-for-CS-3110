type expr = 
  |Flt of float
  |Var of string
  |Add of expr * expr
  |Mult of expr * expr
  |Subt of expr * expr
  |Div of expr * expr