

module Arithmetic_Functions : Arithmetic_Funcs = struct

        let add s = 
                failwith "Unimplemented"

        let subtract s =
                failwith "Unimplemented"

        val multiply 
                failwith "Unimplemented"

        let divide s = 
                failwith "Unimplemented"
end



module Arithmetic_CFU : CFU_sig = struct

        type primitive = float 

        val operation_list = [
                ("+", Arithmetic_Functions.add);
                ("-", Arithmetic_Functions.subtract);
                ("*", Arithmetic_Functions.multiply);
                ("/", Arithmetic_Functions.divide)
        ];
end
