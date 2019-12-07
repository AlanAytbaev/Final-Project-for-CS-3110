open Ast

(** An abstract module type that is meant to structure the CFU modules 
    (ie. Aritmetic functions, calculus functions, statistics functions). 
    A module that matches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig =  sig

  val operation_list : (string * ( value list -> value )) list

end

module type Fib_Funcs = sig

  val nth : value list -> value

  val lst : value list -> value

  val nfib_list : value list -> value

end

module Fib_Functions : Fib_Funcs

module Fib_CFU : CFU_sig