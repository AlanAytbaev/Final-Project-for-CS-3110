(** A module that implements the functions needed for the arithmetic cfu. A modu    le that matches [Arithmetic_Funcs] is suitable for use in [Arithmetic_CFU]. *)
module type Arithmetic_Funcs = sig

        (** [add s] returns the result of adding the first element of s to the
            second element of s. *)
        val add : float list -> float

        (** [subtract s] returns the result of subtracting the first element of             s from the second element of s. *)
        val subtract : float list -> float

        (** [multiply s] returns the result of multiplying the first element of             s by the second element of s. *)
        val multiply : float list -> float

        (** [divide s] returns the result of dividing the first element of s by             the second element of s. *)
        val divide : float list -> float

        val exponentiation : float list -> float

        val modulus : float list -> float

        val logarithm : float list -> float
end

module type CFU_sig = sig

        (** type primitive is the type of the value that the calculator works wi            th*)
        type primitive

        (** An [operation_list] is an association list that maps operation symbo            ls to functions *)
        val operation_list : (string * ( primitive  list -> primitive )) list
end

module Arithmetic_Functions : Arithmetic_Funcs = struct

        let add (s : float list) =
                match s with
                | [] -> failwith "InvalidInput"
                | hd1::hd2::tl -> (Float.add hd1 hd2)

        let subtract (s : float list) =
                match s with
                | [] -> failwith "InvalidInput"
                | hd1::hd2::tl -> (Float.sub hd1 hd2)

        let multiply (s : float list) =
                match s with
                | [] -> failwith "InvalidInput"
                | hd1::hd2::tl -> (Float.mul hd1 hd2)

        let divide (s : float list) =
                match s with
                | [] -> failwith "InvalidInput"
                | hd1::hd2::tl -> (Float.div hd1 hd2)

        let exponentiation (s : float list) =
                match s with
                | [] -> failwith "InvalidInput"
                | hd1::hd2::tl -> (hd1 ** hd2)

        let modulus (s : float list) =
                match s with
                | [] -> failwith "InvalidInput"
                | hd1::hd2::tl ->  (Float.sub hd1 (Float.mul hd2 (Float.div hd1 hd2)))

        let rec log (a : float) (b : float) =
                let n = (Float.compare a b) in
                match n with
                | _ when n < 0 -> 0
                | _ -> 1 + log (Float.div a b) b

        let logarithm (s : float list) =
                match s with
                | [] -> failwith "InvalidInput"
                | hd1::hd2::tl -> (hd1)
end



module Arithmetic_CFU : CFU_sig = struct

        type primitive = float

        let operation_list = [
                ("+", Arithmetic_Functions.add);
                ("-", Arithmetic_Functions.subtract);
                ("*", Arithmetic_Functions.multiply);
                ("/", Arithmetic_Functions.divide);
                ("^", Arithmetic_Functions.exponentiation);
                ("%", Arithmetic_Functions.modulus);
                ("log", Arithmetic_Functions.logarithm)
        ];
end
