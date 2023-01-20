open Transformer_stuff.Micrograd
open Alcotest
open Utils

let a = Value.co ~label:"a" 0.

let b : Value.t =
  {
    value = ref 0.;
    prev = [ a ];
    op = "";
    grad = ref 0.;
    backward = Value.empty.backward;
    label = "b";
    index = Value.get_index ();
  }

let d : Value.t =
  {
    value = ref 0.;
    prev = [ a ];
    op = "";
    grad = ref 0.;
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
  |> check (list @@ string) "a+b -> a,b" [ "(a+b)"; "b"; "a" ]

let test_topological_sort_higher_tree () =
  Utils.topological_sort e
  |> List.map (fun (value : Value.t) -> value.label)
  |> check (list @@ string) "a+[da]->ad" [ "(a+d)"; "d"; "a" ]

let test_backward () =
  let open Value in
  let a = co 2. ~label:"a" in
  a.backward a;
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
  let expected = [ ("c", 1.); ("b", 2.); ("a", 3.) ] in
  let actual =
    Utils.backwards c |> topological_sort
    |> List.map (fun v -> (v.label, !(v.grad)))
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
  let expected = [ ("e", 1.); ("d", 4.); ("c", -2.); ("b", -2.); ("a", -2.) ] in
  let actual =
    Utils.backwards e |> topological_sort
    |> List.map (fun v -> (v.label, !(v.grad)))
  in
  Alcotest.(check' (list (pair string (float 0.5))))
    ~msg:"two layer backwards works" ~expected ~actual

let test_backwards_a_squared () =
  let open Value in
  let a = co 2. ~label:"a" in
  let b = a + a |> relabel "b" in
  let expected = [ ("b", 1.); ("a", 2.) ] in
  let actual =
    Utils.backwards b |> topological_sort
    |> List.map (fun v -> (v.label, !(v.grad)))
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
  let expected =
    [ ("f", 1.); ("e", 1.); ("d", 4.); ("c", -1.); ("b", -1.); ("a", -1.) ]
  in
  let actual =
    Utils.backwards f |> topological_sort
    |> List.map (fun v -> (v.label, !(v.grad)))
  in
  Alcotest.(check' (list (pair string (float Float.epsilon))))
    ~msg:"three layer with double use backwards works" ~expected ~actual

let test_backprop_simple () =
  let open Value in
  let b = co (-3.) ~label:"b" in
  let c = co 10. ~label:"c" in
  let e = a + b |> relabel "e" in
  let d = e + c |> relabel "d" in
  let f = co 2. ~label:"f" in
  let l = d * f |> relabel "l" in
  let expected =
    [
      ("l", 1.); ("f", 7.); ("d", 2.); ("c", 2.); ("e", 2.); ("b", 2.); ("a", 2.);
    ]
  in
  let actual =
    Utils.backwards l |> topological_sort
    |> List.map (fun v -> (v.label, !(v.grad)))
  in
  Alcotest.(check' (list (pair string (float Float.epsilon))))
    ~msg:"simple backprop works" ~expected ~actual

let test_backprop_simple_neuron () =
  let open Value in
  let x1 = co ~label:"x1" 2. in
  let x2 = co ~label:"x2" 0. in
  let w1 = co ~label:"w1" (-3.) in
  let w2 = co ~label:"w2" 1. in
  let b = co ~label:"b" 6.8813735870195432 in
  let x1w1 = x1 * w1 in
  let x2w2 = x2 * w2 in
  let x1w1x2w2 = x1w1 + x2w2 in
  let n = x1w1x2w2 + b in
  let o = tanh n |> relabel "o" in
  let expected =
    [
      ("o", 1.);
      ("(((x1*w1)+(x2*w2))+b)", 0.5);
      ("b", 0.5);
      ("((x1*w1)+(x2*w2))", 0.5);
      ("(x2*w2)", 0.5);
      ("w2", 0.);
      ("x2", 0.5);
      ("(x1*w1)", 0.5);
      ("w1", 1.);
      ("x1", -1.5);
    ]
  in

  let actual =
    Utils.backwards o |> topological_sort
    |> List.map (fun v -> (v.label, !(v.grad)))
  in
  Alcotest.(check' (list (pair string (float 0.01))))
    ~msg:"simple neuron works" ~expected ~actual

let test_backprop_expanded_neuron () =
  let open Value in
  let x1 = co ~label:"x1" 2. in
  let x2 = co ~label:"x2" 0. in
  let w1 = co ~label:"w1" (-3.) in
  let w2 = co ~label:"w2" 1. in
  let b = co ~label:"b" 6.8813735870195432 in
  let x1w1 = x1 * w1 in
  let x2w2 = x2 * w2 in
  let x1w1x2w2 = x1w1 + x2w2 in
  let n = x1w1x2w2 + b in
  let e = exp (co 2. * n) in
  let o = (e - co 1.) / (e + co 1.) in
  let expected =
    [
      ( "((exp ((2.*(((x1*w1)+(x2*w2))+b)))+(1.*-1.))*((exp \
         ((2.*(((x1*w1)+(x2*w2))+b)))+1.)**-1.))",
        1. );
      ("((exp ((2.*(((x1*w1)+(x2*w2))+b)))+1.)**-1.)", 4.82843);
      ("-1.", 0.707107);
      ("(exp ((2.*(((x1*w1)+(x2*w2))+b)))+1.)", -0.103553);
      ("1.", -0.103553);
      ("(exp ((2.*(((x1*w1)+(x2*w2))+b)))+(1.*-1.))", 0.146447);
      ("(1.*-1.)", 0.146447);
      ("-1.", 0.146447);
      ("1.", -0.146447);
      ("exp ((2.*(((x1*w1)+(x2*w2))+b)))", 0.0428932);
      ("(2.*(((x1*w1)+(x2*w2))+b))", 0.25);
      ("(((x1*w1)+(x2*w2))+b)", 0.5);
      ("b", 0.5);
      ("((x1*w1)+(x2*w2))", 0.5);
      ("(x2*w2)", 0.5);
      ("w2", 0.);
      ("x2", 0.5);
      ("(x1*w1)", 0.5);
      ("w1", 1.);
      ("x1", -1.5);
      ("2.", 0.220343);
    ]
  in
  let actual =
    Utils.backwards o |> topological_sort
    |> List.map (fun v -> (v.label, !(v.grad)))
  in
  Alcotest.(check' (list (pair string (float 0.1))))
    ~msg:"neuron with expanded tanh works" ~expected ~actual

let tests =
  [
    ("topological sort", `Quick, test_topological_sort_c);
    ("topological sort linked list", `Quick, test_topological_sort_linked_list);
    ("topological sort bin tree", `Quick, test_topological_sort_bin_tree);
    ("topological sort higher tree", `Quick, test_topological_sort_higher_tree);
    ("backwards 1 layer", `Quick, test_backwards_1_layer);
    ("backwards 2 layer", `Quick, test_backwards_2_layer);
    ("backwards use twice", `Quick, test_backwards_a_squared);
    ("backwards use twice 3 layer", `Quick, test_backwards_use_twice_3_layer);
    ("backprop simple", `Quick, test_backprop_simple);
    ("backprop simple neuron", `Quick, test_backprop_simple_neuron);
    ("backprop expanded neuron", `Quick, test_backprop_expanded_neuron);
  ]
