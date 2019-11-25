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

module type Statistics_Funcs = sig
  val mean : float list -> float
  val median : float list -> float
  val standard_deviation : float list -> float
  val range : float list -> float
  val min : float list -> float
  val max : float list -> float
  val permutations : float list-> float
  val combinations : float list ->  float
  val factorial : float -> float
end

module Statistics_Functions : Statistics_Funcs
module Statistics_CFU : CFU_sig