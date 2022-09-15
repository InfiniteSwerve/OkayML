open Alcotest
(* open Matrices *)

let test_suites = 
  [("matrices", Matrices.tests)]
let () = run "proj" test_suites
