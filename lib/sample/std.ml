open! Core.Std 
open Async.Std

let do_something_awesome () =
  Print.printf "Everything works!\n"
;;

(* sample function*)
let foo x = x + 5 

(* sample unit test *)
open OUnit
TEST_MODULE "value" = struct
    TEST_UNIT = assert_equal (foo 5) 10
end
