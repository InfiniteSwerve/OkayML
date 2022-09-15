open Alcotest
open Transformer_stuff.Matrices

let test_dot_product () =
  dot_product [|1.;1.|] [|1.;1.|]|> check (float 0.1) "works at all" 2.

let test_dot_product_small () = 
  dot_product [|1.;|] [|1.;|] |> check (float 0.1) "works small" 1.

let test_dot_product_big () = 
  let arr = Array.make 1000 1. in 
  dot_product arr arr |> check (float 0.1) "works small" 1000.

let twos = make ~initialize:(fun _ -> 1.) 2 2 

let tests = 
  [
    ("dot product", `Quick, test_dot_product);
    ("dot product", `Quick, test_dot_product_small);
    ("dot product", `Quick, test_dot_product_big);
  ]
