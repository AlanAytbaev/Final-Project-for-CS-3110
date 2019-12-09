open Ast

(** A module that implements the functions needed for the arithmetic cfu.
    A module that matches [Arithmetic_Funcs] is suitable for use in
    [Arithmetic_CFU]. *)
module type Arithmetic_Funcs = sig
  val add : value list -> value
  val subtract : value list -> value
  val multiply : value list -> value
  val divide : value list -> value
  val exponentiation : value list -> value
  val modulus : value list -> value
  val logarithm : value list -> value
  val equal_to : value list -> value
end

module type CFU_sig = sig
  (** An [operation_list] is an association list that maps operation symbols
      to functions *)
  val operation_list : (string * ( value list -> value )) list
  (** [find s] is the operation that is associated with [s] in the operation
      list *)
  val find_function : string -> (value list -> value)
end

module Arithmetic_Functions : Arithmetic_Funcs = struct

  (** [unwrap v] is the float extracted from value [v] 
      requires: [v] has type VFloat *)
  let unwrap v =
    match v with
    | VFloat x -> x
    | _ -> failwith "This cannot occur - arithmetic.ml"

  let add (s : value list) =
    match s with
    | hd1::hd2::tl -> 
      let (hd1', hd2') = (unwrap hd1, unwrap hd2) in
      VFloat (Float.add hd1' hd2')
    | _ -> failwith "InvalidInput"

  let subtract (s : value list) =
    match s with
    | hd1::hd2::tl -> 
      let (hd1', hd2') = (unwrap hd1, unwrap hd2) in
      VFloat (Float.sub hd1' hd2')
    | _ -> failwith "InvalidInput"

  let multiply (s : value list) =
    match s with
    | hd1::hd2::tl -> 
      let (hd1', hd2') = (unwrap hd1, unwrap hd2) in
      VFloat (Float.mul hd1' hd2')
    | _ -> failwith "InvalidInput"

  let divide (s : value list) =
    match s with
    | hd1::hd2::tl -> 
      let (hd1', hd2') = (unwrap hd1, unwrap hd2) in
      VFloat (Float.div hd1' hd2')
    | _ -> failwith "InvalidInput"

  let exponentiation (s : value list) =
    match s with
    | hd1::hd2::tl -> 
      let (hd1', hd2') = (unwrap hd1, unwrap hd2) in
      VFloat (hd1' ** hd2')
    | _ -> failwith "InvalidInput"

  let modulus (s : value list) =
    match s with
    | hd1::hd2::tl ->  
      let (hd1', hd2') = (unwrap hd1, unwrap hd2) in
      VFloat (Stdlib.mod_float hd1' hd2')
    | _ -> failwith "InvalidInput"

  (** [log a b] computes the logarithm of [a] with base [b] *)
  let rec log (a : float) (b : float) =
    if a > 1. then 1.0 +. log (Float.div a b) b else 0.

  let logarithm (s : value list) =
    match s with
    | hd1::hd2::tl -> 
      let (hd1', hd2') = (unwrap hd1, unwrap hd2) in
      VFloat (log hd1' hd2')
    | _ -> failwith "InvalidInput"

  let equal_to (s : value list) =
    match s with
    | hd1::hd2::tl -> 
      let (hd1', hd2') = (unwrap hd1, unwrap hd2) in
      VFloat (if (hd1' = hd2') then 1.0 else 0.0)
    | _ -> failwith "InvalidInput"
end

module Arithmetic_CFU : CFU_sig = struct

  let operation_list = [
    ("+", Arithmetic_Functions.add);
    ("-", Arithmetic_Functions.subtract);
    ("*", Arithmetic_Functions.multiply);
    ("/", Arithmetic_Functions.divide);
    ("^", Arithmetic_Functions.exponentiation);
    ("%", Arithmetic_Functions.modulus);
    ("log", Arithmetic_Functions.logarithm);
    ("==", Arithmetic_Functions.equal_to)
  ]

  let find_function (identifier : string) =
    match List.assoc_opt identifier operation_list with
    |Some f -> f
    |None -> failwith (identifier^" is not a valid imported function")

end
