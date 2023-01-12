open Transformer_stuff.Micrograd
open Alcotest
open Utils

let value_testable = testable Value.pp_value Value.eq
let a = Value.co ~label:"a" 0.

let b : Value.t =
  {
    value = 0.;
    prev = [ a ];
    op = "";
    grad = 0.;
    backward = Value.empty.backward;
    label = "b";
    index = Value.get_index ();
  }

let d : Value.t =
  {
    value = 0.;
    prev = [ a ];
    op = "";
    grad = 0.;
    backward = Value.empty.backward;
    label = "d";
    index = Value.get_index ();
  }

let c = Value.(a + co ~label:"b" 0.)
let e = Value.(a + d)

let test_topological_sort_c () =
  Utils.topological_sort a
  |> List.map (fun (value : Value.t) -> value.label)
  |> check (list @@ string) "1 c" [ "a" ]

let test_topological_sort_linked_list () =
  Utils.topological_sort b
  |> List.map (fun (value : Value.t) -> value.label)
  |> check (list @@ string) "a -> b" [ "b"; "a" ]

let test_topological_sort_bin_tree () =
  Utils.topological_sort c
  |> List.map (fun (value : Value.t) -> value.label)
  |> check (list @@ string) "a+b -> a,b" [ "a+b"; "b"; "a" ]

let test_topological_sort_higher_tree () =
  Utils.topological_sort e
  |> List.map (fun (value : Value.t) -> value.label)
  |> check (list @@ string) "a+[da]->ad" [ "a+d"; "d"; "a" ]

let test_backward () =
  let open Value in
  let a = co 2. ~label:"a" in
  let a = a.backward a in
  check string "a.backward.label = a.label" "a" a.label

(*
     c(+)
    / \
  a=2 b=3
*)
let test_backwards_1_layer () =
  let open Value in
  let a = co 2. ~label:"a" in
  let b = co 3. ~label:"b" in
  let c = a * b |> relabel "c" in
  let expected = [ ("a", 3.); ("b", 2.); ("c", 1.) ] in
  let actual =
    Utils.(backwards c |> extract_value_to_list)
    |> List.map (fun v -> (v.label, v.grad))
  in
  Alcotest.(check' (list (pair string (float 0.5))))
    ~msg:"a + b = c" ~expected ~actual

let test_backwards_2_layer () =
  let open Value in
  let a = co 2. ~label:"a" in
  let b = co 2. ~label:"b" in
  let c = a + b |> relabel "c" in
  let d = co (-2.) ~label:"d" in
  let e = c * d |> relabel "e" in
  let expected = [ ("a", -2.); ("b", -2.); ("c", -2.); ("d", 4.); ("e", 1.) ] in
  let actual =
    Utils.backwards e |> Utils.extract_value_to_list
    |> List.map (fun v -> (v.label, v.grad))
  in
  Alcotest.(check' (list (pair string (float 0.5))))
    ~msg:"two layer backwards works" ~expected ~actual

let test_backwards_a_squared () =
  let open Value in
  let a = co 2. ~label:"a" in
  let b = a + a |> relabel "b" in
  let expected = [] in
  let actual =
    Utils.backwards b |> Utils.extract_value_to_list
    |> List.map (fun v -> (v.label, v.grad))
  in
  Alcotest.(check' (list (pair string (float 0.5))))
    ~msg:"same variable can appear twice" ~expected ~actual

let test_backwards_use_twice_3_layer () =
  let open Value in
  let a = co 2. ~label:"a" in
  let b = co 2. ~label:"b" in
  let c = a + b |> relabel "c" in
  let d = co (-2.) ~label:"d" in
  let e = c * d |> relabel "e" in
  let f = e + c |> relabel "f" in
  let expected = [] in
  let actual =
    Utils.backwards f |> Utils.extract_value_to_list
    |> List.map (fun v -> (v.label, v.grad))
  in
  Alcotest.(check' (list (pair string (float Float.epsilon))))
    ~msg:"three layer with double use backwards works" ~expected ~actual

(* let test_backprop_simple () = *)
(*   let open Value in *)
(*   let b = c (-3.) ~label:"b" in *)
(*   let c' = c 10. ~label:"c" in *)
(*   let e = a + b |> relabel "e" in *)
(*   let d = e + c' |> relabel "d" in *)
(*   let f = c 2. ~label:"f" in *)
(*   let l = d * f in *)
(*   Utils.sort_print l; *)
(*   () *)

let test_backprop_simple_neuron () =
  let open Value in
  let x1 = co ~label:"x1" 2. in
  let x2 = co ~label:"x2" 0. in
  let w1 = co ~label:"w1" (-3.) in
  let w2 = co ~label:"w2" 1. in
  let b = co ~label:"b" 6.8813735870195432 in
  let x1w1 = x1 * w1 |> relabel "x1w1" in
  let x2w2 = x2 * w2 |> relabel "x2w2" in
  let x1w1x2w2 = x1w1 + x2w2 |> relabel "x1w1x2w2" in
  let n = x1w1x2w2 + b |> relabel "n" in
  let o = tanh n |> relabel "o" in
  Utils.sort_print o;
  ()

let test_backprop_expanded_neuron () =
  let open Value in
  let x1 = co ~label:"x1" 2. in
  let x2 = co ~label:"x2" 0. in
  let w1 = co ~label:"w1" (-3.) in
  let w2 = co ~label:"w2" 1. in
  let b = co ~label:"b" 6.8813735870195432 in
  let x1w1 = x1 * w1 |> relabel "x1w1" in
  let x2w2 = x2 * w2 |> relabel "x2w2" in
  let x1w1x2w2 = x1w1 + x2w2 |> relabel "x1w1x2w2" in
  let n = x1w1x2w2 + b |> relabel "n" in
  let e = exp (co 2. * n) |> relabel "e" in
  let o = (e - co 1.) / (e + co 1.) |> relabel "o" in
  Utils.sort_print o;
  ()

let tests =
  [
    ("topological sort", `Quick, test_topological_sort_c);
    ("topological sort", `Quick, test_topological_sort_linked_list);
    ("topological sort", `Quick, test_topological_sort_bin_tree);
    ("topological sort", `Quick, test_topological_sort_higher_tree);
    (* ("topological sort", `Quick, test_Utils.topological_sort_loop); *)
    ("backward", `Quick, test_backwards_1_layer);
    ("backward", `Quick, test_backwards_2_layer);
    ("backward", `Quick, test_backwards_a_squared);
    ("backward", `Quick, test_backwards_use_twice_3_layer);
  ]
