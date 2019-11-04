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
  |Bool of bool
  |Let of string * expr
  |If of expr * expr * expr
