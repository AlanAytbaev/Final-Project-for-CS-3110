open Ast

module type Main_Sig = sig
  val interp : string -> env -> (string * env)
  val run : unit -> unit
  val initial_env : env
end

module Main : Main_Sig