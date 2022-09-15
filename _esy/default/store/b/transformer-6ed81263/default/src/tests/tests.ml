open Alcotest
(* open Matrices *)

let () = 
  Micrograd.test_backprop_simple () 

let test_suites = 
  [("matrices", Matrices.tests);
   ("micrograd", Micrograd.tests);]
let () = run "proj" test_suites
