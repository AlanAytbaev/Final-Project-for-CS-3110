(* * An abstract module type that is meant to structure the CFU modules (ie. Arit    metic functions, calculus functions, statistics functions). A module that ma    tches [CFU_sig] is suitable for use in the [Calc] module.
   module type CFU_sig = sig

        (** type primitive is the type of the value that the calculator works wi            th*)
        type primitive

        (** An [operation_list] is an association list that maps operation symbo            ls to functions *)
        val operation_list : (string * ( primitive  list -> primitive )) list
   end



   module Make:functor (C:CFU_sig)->
   functor (Environment:Environment_sig) ->
   functor (Parser:Parses_sig) -> Calculator with module Environment = E *)
