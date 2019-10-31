
module Arithmetic_Functions : Arithmetic_Funcs = struct

        let add s =
                failwith "Unimplemented"

        let subtract s =
                failwith "Unimplemented"

        let multiply s =
                failwith "Unimplemented"

        let divide s =
                failwith "Unimplemented"

        let exponentiation s =
                failwith "Unimplemented"

        let modulus s =
                failwith "Unimplemented"

        let logarithm s =
                failwith "Unimplemented"
end



module Arithmetic_CFU : CFU_sig = struct

        type primitive = float

        let operation_list = [
                ("+", Arithmetic_Functions.add);
                ("-", Arithmetic_Functions.subtract);
                ("*", Arithmetic_Functions.multiply);
                ("/", Arithmetic_Functions.divide)
        ];
end
