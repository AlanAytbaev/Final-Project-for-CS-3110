(** An abstract module type that is meant to structure the CFU modules (ie. Arit    metic functions, calculus functions, statistics functions). A module that ma    tches [CFU_sig] is suitable for use in the [Calc] module. *)
module type CFU_sig = sig

  (** type primitive is the type of the value that the calculator works wi            th*)
  type primitive

  (** An [operation_list] is an association list that maps operation symbo            ls to functions *)
  val operation_list : (string * ( primitive  list -> primitive )) list

  (** [find s] is the operation that is associated with [s] in the operation
      list *)
  val find_function : string -> (float list -> float)
end


(** A module that implements the functions needed for the trigonometric cfu. A module
    that matches [Trigonometric_Funcs] is suitable for use in [Trigonometric_CFU]. *)
module type Trigonometric_Funcs = sig

  val deg_to_rad : float list -> float

  val rad_to_deg : float list -> float

  val sin : float list -> float

  val cos : float list -> float

  val tan : float list -> float

  val sec : float list -> float

  val cosec : float list -> float

  val cotan : float list -> float

end

module Trigonometric_Functions : Trigonometric_Funcs
module Trigonometric_CFU : CFU_sig
