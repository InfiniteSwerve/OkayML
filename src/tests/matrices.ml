open Alcotest
open Transformer_stuff.Matrices

let co = Transformer_stuff.Micrograd.Value.co

let test_dot_product () =
  let v = Vec.make 2 (co 1.) in
  let o = Vec.dot_product v v in
  check (float 0.1) "works at all" 2. !(o.value)

let test_dot_product_small () =
  let v = Vec.make 1 (co 1.) in
  let o = Vec.dot_product v v in
  check (float 0.1) "works at all" 1. !(o.value)

let test_dot_product_big () =
  let v = Vec.make 1000 (co 1.) in
  let o = Vec.dot_product v v in
  check (float 0.1) "big" 1000. !(o.value)

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
