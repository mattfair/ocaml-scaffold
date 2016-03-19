open! Core.Std 
open Async.Std

let do_something_awesome () =
  Print.printf "Everything works!\n"
;;

(* sample function*)
let foo x = x + 5 

(* sample unit test *)
open OUnit
let%test_module "value" = (module struct
    let%test_unit _ = assert_equal (foo 5) 10
end)
