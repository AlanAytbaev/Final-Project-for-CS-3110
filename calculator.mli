module Make:functor (C:CFU_sig)-> 
  functor (Environment:Environment_sig) -> 
  functor (Parser:Parses_sig) -> Calculator with module Environment = E