(** A module that implements the functions needed for the arithmetic cfu.
    A module that matches [Arithmetic_Funcs] is suitable for use in
    [Arithmetic_CFU]. *)
module type Arithmetic_Funcs = sig
  val add2 : float list -> float
  val subtract : float list -> float
  val multiply : float list -> float
  val divide : float list -> float
  val exponentiation : float list -> float
  val modulus : float list -> float
  val logarithm : float list -> float
end

module type CFU_sig = sig
  (** type primitive is the type of the value that the calculator works with*)
  type primitive
  (** An [operation_list] is an association list that maps operation symbols
      to functions *)
  val operation_list : (string * ( primitive  list -> primitive )) list
  (** [find s] is the operation that is associated with [s] in the operation
      list *)
  val find_function : string -> (float list -> float)
end

module Arithmetic_Functions : Arithmetic_Funcs = struct

  let add2 (s : float list) =
    match s with
    | hd1::hd2::tl -> (Float.add hd1 hd2)
    | _ -> failwith "InvalidInput"

  let subtract (s : float list) =
    match s with
    | hd1::hd2::tl -> (Float.sub hd1 hd2)
    | _ -> failwith "InvalidInput"

  let multiply (s : float list) =
    match s with
    | hd1::hd2::tl -> (Float.mul hd1 hd2)
    | _ -> failwith "InvalidInput"

  let divide (s : float list) =
    match s with
    | hd1::hd2::tl -> (Float.div hd1 hd2)
    | _ -> failwith "InvalidInput"

  let exponentiation (s : float list) =
    match s with
    | hd1::hd2::tl -> (hd1 ** hd2)
    | _ -> failwith "InvalidInput"

  let modulus (s : float list) =
    match s with
    | hd1::hd2::tl ->  Stdlib.mod_float hd1 hd2
    | _ -> failwith "InvalidInput"

  let rec log (a : float) (b : float) =
    let n = (Float.compare a b) in
    match n with
    | _ when n < 0 -> 0.0
    | _ -> 1.0 +. log (Float.div a b) b

  let logarithm (s : float list) =
    match s with
    | hd1::hd2::tl -> log hd1 hd2
    | _ -> failwith "InvalidInput"
end

module Arithmetic_CFU : CFU_sig = struct

  type primitive = float

  let operation_list = [
    ("+", Arithmetic_Functions.add2);
    ("-", Arithmetic_Functions.subtract);
    ("*", Arithmetic_Functions.multiply);
    ("/", Arithmetic_Functions.divide);
    ("^", Arithmetic_Functions.exponentiation);
    ("%", Arithmetic_Functions.modulus);
    ("log", Arithmetic_Functions.logarithm)
  ]

  let find_function (identifier : string) =
    match List.assoc_opt identifier operation_list with
    |Some f -> f
    |None -> failwith (identifier^" is not a valid imported function")

end
