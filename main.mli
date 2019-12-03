open Ast

module type Main_Sig = sig
  val interp : string -> env -> (string * env)
  val run : unit -> unit
end

module Main : Main_Sig