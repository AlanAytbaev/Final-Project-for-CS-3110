type bop =
  |Add
  |Mult
  |Subt
  |Div
  |Exp
  |Mod
  |Log
  |Equals
  |Func of string

type expr =
  |Var of string
  |Float of float
  |Binop of bop * expr * expr
  |Boolean of bool
  |Let of string * expr
  |If of expr * expr * expr
