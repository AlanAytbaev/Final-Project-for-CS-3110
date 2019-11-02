open OUnit2
open Ast
open Main
open Arithmetic_Functions

(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [Int i]. *)
let make_i n i s =
  n >:: (fun _ -> assert_equal (string_of_float i) (interp s))

(* Add Tests Below Here *)

let tests = [
  make_i "float" 22. "22";
  make_i "add" 22. "11+11";
  make_i "mul1" 22. "2*11";
  make_i "mul2" 22. "2+2*10";
  make_i "mul3" 14. "2*2+10";
  make_i "mul4" 40. "2*2*10";

]

(**
let test_arithmetic =
        assert_equal 3. (add [1.; 2.])
        assert_equal 1. (subtract [2.; 1.])
        assert_equal 4. (multiply [2.; 2.])
        assert_equal 1. (divide [1.; 1.])
        assert_equal 4. (exponentiation [2.; 2.])
        assert_equal 0. (modulus [6.; 6.])
        assert_equal 1. (logarithm [1.; 1.])
*)


let suite = "calc test suite" >::: tests

let _ = run_test_tt_main suite
