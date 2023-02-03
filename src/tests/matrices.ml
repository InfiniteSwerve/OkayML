open Alcotest
open Transformer_stuff.Matrices
open Vec

let test_dot_product () =
  dot_product [| 1.; 1. |] [| 1.; 1. |] |> check (float 0.1) "works at all" 2.

let test_dot_product_small () =
  dot_product [| 1. |] [| 1. |] |> check (float 0.1) "works small" 1.

let test_dot_product_big () =
  let arr = Array.make 1000 1. in
  dot_product arr arr |> check (float 0.1) "works small" 1000.

let test_mapi () =
  let m = Mat.zeros 2 2 in
  let expected = [ [ (1, 1); (1, 2) ]; [ (2, 1); (2, 2) ] ] in
  let actual = Mat.mapi (fun (x, y) _ -> (x + 1, y + 1)) m in
  let actual = Mat.to_row_list actual in
  let actual = List.map Vec.to_list actual in
  Alcotest.(check' (list (list (pair int int))))
    ~msg:"mapi works correctly" ~expected ~actual

(* let twos = Mat.make ~initialize:(fun _ -> 1.) 2 2 *)

(* let test_matrix_prod () = *)
(*   twos @ twos |> check (array @@ array @@ float 0.1) "works small" *)

let tests =
  [
    ("dot product", `Quick, test_dot_product);
    ("dot product", `Quick, test_dot_product_small);
    ("dot product", `Quick, test_dot_product_big);
    ("matrices", `Quick, test_mapi);
  ]
