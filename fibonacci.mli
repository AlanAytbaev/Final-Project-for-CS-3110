open Ast

(** An abstract module type that is meant to structure the CFU modules 
    (ie. Aritmetic functions, calculus functions, statistics functions). 
    A module that matches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig =  sig

  (** An [operation_list] is an association list that maps operation 
        symbols to functions *)
  val operation_list : (string * ( value list -> value )) list

end

module type Fib_Funcs = sig

  (** [nth n] returns the value at position [n] in the fibonacci sequence *)
  val nth : value list -> value

  (**  [lst n] gives the first [n] values in the fibonacci sequence  *)
  val lst : value list -> value

  (** [nfib_list b n] is the first [n] elements of of b-nacci sequence *)
  val nfib_list : value list -> value

end

module Fib_Functions : Fib_Funcs

module Fib_CFU : CFU_sig