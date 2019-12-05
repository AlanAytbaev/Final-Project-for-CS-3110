open Ast

(** An abstract module type that is meant to structure the CFU modules (ie. Arit    metic functions, calculus functions, statistics functions). A module that ma    tches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig = sig

  (** An [operation_list] is an association list that maps operation symbo            ls to functions *)
  val operation_list : (string * ( value  list -> value )) list

  (** [find s] is the operation that is associated with [s] in the operation
      list *)
  val find_function : string -> (value list -> value)
end

(** A module that implements the functions needed for the arithmetic cfu. A modu    le that matches [Arithmetic_Funcs] is suitable for use in [Arithmetic_CFU]. *)
module type Arithmetic_Funcs = sig
  (** [add s] returns the result of adding the first element of s to the
      second element of s.*)

  val add2 : value list -> value

  (** [subtract s] returns the result of subtracting the first element of             s from the second element of s. *)
  val subtract : value list -> value

  (** [multiply s] returns the result of multiplying the first element of             s by the second element of s. *)
  val multiply : value list -> value

  (** [divide s] returns the result of dividing the first element of s by             the second element of s. *)
  val divide : value list -> value

  val exponentiation : value list -> value

  val modulus : value list -> value

  val logarithm : value list -> value

  val equal_to : value list -> value
end

module Arithmetic_Functions : Arithmetic_Funcs
module Arithmetic_CFU : CFU_sig
