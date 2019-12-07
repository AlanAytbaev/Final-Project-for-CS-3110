open Ast

(** An abstract module type that is meant to structure the CFU modules (ie. Arit    metic functions, calculus functions, statistics functions). A module that ma    tches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig = sig

  val operation_list : (string * ( value list -> value)) list

end

module type Set_Funcs = sig

  val intersect : value list -> value 
  val union : value list -> value 
  val difference : value list -> value 

end

module Set_Functions : Set_Funcs
module Set_CFU : CFU_sig