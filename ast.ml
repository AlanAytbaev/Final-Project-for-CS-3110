type bop =
  |Add
  |Mult
  |Subt
  |Div
  |Func of string


type expr =
  |Var of string
  |Float of float
  |Binop of bop * expr * expr
  |Let of string * expr * expr