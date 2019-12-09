open Ast

(** An abstract module type that is meant to structure the CFU modules
    (ie. Aritmetic functions, calculus functions, statistics functions).
    A module that matches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig = sig

  (** An [operation_list] is an association list that maps operation 
        symbols to functions *)
  val operation_list : (string * ( value list -> value)) list

end

module type MySet_Funcs = sig

  (** [interesect s1 s2] is the array containing the elements in both [s1] and
          [s2] *)
  val intersect : value list -> value

  (** [union s1 s2] is the array containing the elements in [s1] or in [s2]
      Note: result contains no duplicates *)
  val union : value list -> value

  (** [difference s1 s2] is the array containing the elements in [s1] and
        not in [s2] *)
  val difference : value list -> value

  val append_val : value list -> value

  val remove : value list -> value

  val remove_element : value list -> value

  val length : value list -> value

  val get_element : value list -> value

end

module MySet_Functions : MySet_Funcs
module MySet_CFU : CFU_sig
