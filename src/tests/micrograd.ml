open Transformer_stuff.Micrograd
open Alcotest

let a = Value.c ~label:"a" 0.
let b : Value.t = {
  value=0.;
  prev = [a];
  op = "";
  grad = 0.;
  backward = (Value.empty 0.).backward;
  label = "b";
  index = Value.get_index ()
  }
  
let d : Value.t = {
  value=0.;
  prev = [a];
  op = "";
  grad = 0.;
  backward = (Value.empty 0.).backward;
  label = "d";
  index = Value.get_index ()
  }

let c = Value.(a + (c ~label:"b" 0.))
let e = Value.(a + d)
let test_topological_sort_c () =
  topological_sort a
  |> List.map (fun (value : Value.t) -> value.label)
  |> check (list @@ string) "1 c" ["a"]

let test_topological_sort_linked_list () =
  topological_sort b
  |> List.map (fun (value : Value.t) -> value.label)
  |> check (list @@ string) "a -> b" ["b";"a"]

let test_topological_sort_bin_tree () =
  topological_sort c
  |> List.map (fun (value : Value.t) -> value.label)
  |> check (list @@ string) "a+b -> a,b" ["ab";"b";"a"]

let test_topological_sort_higher_tree () =
  topological_sort e
  |> List.map (fun (value : Value.t) -> value.label)
  |> check (list @@ string) "a+[da]->ad" ["ad";"d";"a"]
  
let test_backprop_simple () = 
  let open Value in 
  let x1 = c ~label:"x1" 2. in 
  let x2 = c ~label:"x2" 0. in 
  let w1 = c ~label:"w1" (-3.) in 
  let w2 = c ~label:"w2" 1. in 
  let b = c ~label:"b" 6.88 in 
  let x1w1 = x1 * w1 |> relabel "x1w1" in 
  let x2w2 = x2 * w2 |> relabel "x2w2" in 
  let x1w1x2w2 = x1w1 + x2w2 |> relabel "x1w1x2w2" in 
  let n = x1w1x2w2 + b |> relabel "n" in 
  let o = tanh n |> relabel "o" in
  let o = {o with grad = 1.} in 
  let o = o.backward () |> relabel "o" in 
  let grads = topological_sort o |> List.map (fun value -> (value.grad, value.label)) in 
  List.iter (fun (value, label) -> Format.eprintf "%f, %s \n%!" value label) grads ;
  ()
  


(* let test_topological_sort_loop () =
  let a = {a with prev=[b]} 
  and b = {b with prev=[a]} in 
  topological_sort a
  |> List.map (fun (value : Value.t) -> value.label)
  |> check (list @@ string) "a -> a -> a" ["a"] *)


let tests = [
  ("topological sort", `Quick, test_topological_sort_c);
  ("topological sort", `Quick, test_topological_sort_linked_list);
  ("topological sort", `Quick, test_topological_sort_bin_tree);
  ("topological sort", `Quick, test_topological_sort_higher_tree);
  (* ("topological sort", `Quick, test_topological_sort_loop); *)
  ]
