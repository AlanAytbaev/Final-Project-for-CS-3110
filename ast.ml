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
  |If of expr * expr * expr
  |Let of string * expr * expr
  |Fun of string list * expr
  |FunApp of expr * expr list


type defn = 
  |DLet of string * expr

type phrase = 
  | Expr of expr
  | Defn of defn