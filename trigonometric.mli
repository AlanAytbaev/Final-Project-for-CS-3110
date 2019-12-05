open Ast

(** An abstract module type that is meant to structure the CFU modules (ie. Arit    metic functions, calculus functions, statistics functions). A module that ma    tches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig = sig

  (** An [operation_list] is an association list that maps operation symbo            ls to functions *)
  val operation_list : (string * ( value  list -> value )) list

  (** [find s] is the operation that is associated with [s] in the operation
      list *)
  val find_function : string -> (value list -> value)
end


(** A module that implements the functions needed for the trigonometric cfu. A module
    that matches [Trigonometric_Funcs] is suitable for use in [Trigonometric_CFU]. *)
module type Trigonometric_Funcs = sig

  val deg_to_rad : value list -> value

  val rad_to_deg : value list -> value

  val sin : value list -> value

  val cos : value list -> value

  val tan : value list -> value

  val sec : value list -> value

  val cosec : value list -> value

  val cotan : value list -> value

end

module Trigonometric_Functions : Trigonometric_Funcs
module Trigonometric_CFU : CFU_sig
