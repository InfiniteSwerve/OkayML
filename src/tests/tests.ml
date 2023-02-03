open Alcotest
(* open Matrices *)

(* let () =  *)
(* (* Micrograd.test_backprop_super_simple (); *)
   (*   Micrograd.test_backprop_simple (); *) *)
(*   Micrograd.test_backprop_expanded_neuron ()  *)

let test_suites =
  [
    ("matrices", Matrices.tests);
    ("micrograd", Micrograd.tests);
    ("neural networks", Neural_nets.tests);
    ("stats", Stats.tests);
  ]

let () = run "proj" test_suites
