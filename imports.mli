open Ast

module type Imports_Sig = sig
  (** [find s] is the operation that is associated with [s] in the operation
      list *)
  val find_function : string -> (value list -> value)
  val functions_map : env
end
module Imports : Imports_Sig