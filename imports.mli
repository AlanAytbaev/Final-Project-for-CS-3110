open Ast

module type Imports_Sig = sig

  (** [find_function s] is the operation that is associated with [s] 
      in the operationlist *)
  val find_function : string -> (value list -> value)

  (** DOCUMENT ME *)
  val functions_map : env
end
module Imports : Imports_Sig