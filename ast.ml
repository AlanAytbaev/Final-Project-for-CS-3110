
type id = string
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

module Env = Map.Make (String)

type env = expr Env.t
and expr =
  |Var of id
  |Float of float
  |Binop of bop * expr * expr
  |Boolean of bool
  |If of expr * expr * expr
  |Let of id * expr * expr
  |Fun of string list * expr
  |FunApp of expr * expr list
  (* |Closure of expr * env *)


and defn = 
  |DLet of string * expr

and phrase = 
  | Expr of expr
  | Defn of defn