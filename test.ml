open OUnit2

open Arithmetic_Functions

let tests = []

let suite = "calc test suite" >::: tests

let _ = run_test_tt_main suite

(* Add Tests Below Here *)

let t

let test_arithmetic =
        assert_equal 3.0 (add [1.0; 2.0])
        assert_equal 1.0 (subtract [2.0; 1.0])
        assert_equal 4.0 (multiply [2.0; 2.0])
        assert_equal 1.0 (divide [1.0; 1.0])
        assert_equal 4.0 (exponentiation [2.0; 2.0])
        assert_equal 0.0 (modulus [6.0; 6.0])
        assert_equal 1.0 (logarithm [1.0; 1.0])
