open Ast

module type CFU_sig = sig


  (** An [operation_list] is an association list that maps operation 
        symbols to functions *)
  val operation_list : (string * ( value list -> value)) list

end

module type NewSet_Funcs = sig

  (** [isEqual x y] is true if two sets contain structurally 
       equal elements and have the same number of elements and 
       is false otherwise*)
  val isEqual : value list -> value 

  (** [isSubset x y] is true is x is a subset of y, false otherwise *)
  val isSubset : value list -> value 

  (** [isDisjoint x y] is true if none of the elements in [x] are
      in [y] and is false otherwise*)  
  val isDisjoint : value list -> value 
end

module NewSet_Functions : NewSet_Funcs
module NewSet_CFU : CFU_sig