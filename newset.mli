open Ast

module type CFU_sig = sig

  val operation_list : (string * ( value list -> value)) list

end

module type NewSet_Funcs = sig
  val isEqual : value list -> value 
  val isSubset : value list -> value   
  val isDisjoint : value list -> value 
end

module NewSet_Functions : NewSet_Funcs
module NewSet_CFU : CFU_sig