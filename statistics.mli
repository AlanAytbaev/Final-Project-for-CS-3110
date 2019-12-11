open Ast

(** An abstract module type that is meant to structure the CFU modules (ie. Arit    metic functions, calculus functions, statistics functions). A module that ma    tches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig = sig

  (** An [operation_list] is an association list that maps operation symbo            ls to functions *)
  val operation_list : (string * ( value list -> value )) list

  (** [find s] is the operation that is associated with [s] in the operation
      list *)
  val find_function : string -> (value list -> value)
end

module type Statistics_Funcs = sig
  val mean : value list -> value
  val median : value list -> value
  val standard_deviation : value list -> value
  val range : value list -> value
  val minimum : value list -> value
  val maximum : value list -> value
  val permutations : value list -> value
  val combinations : value list -> value
end

module Statistics_Functions : Statistics_Funcs
module Statistics_CFU : CFU_sig
