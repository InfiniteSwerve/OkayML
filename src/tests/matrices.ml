open Alcotest
open Transformer_stuff.Matrices
module FVec = FTensor.Vec
module FMat = FTensor.Mat
module VVec = VTensor.Vec
module VMat = VTensor.Mat

let co = Transformer_stuff.Micrograd.Value.co

let test_dot_product () =
  let v = VVec.make ~initialize:(fun _ -> co 1.) 2 in
  let o = VVec.dot_product v v in
  check (float 0.1) "works at all" 2. !(o.value)

let test_dot_product_small () =
  let v = VVec.make ~initialize:(fun _ -> co 1.) 1 in
  let o = VVec.dot_product v v in
  check (float 0.1) "works at all" 1. !(o.value)

let test_dot_product_big () =
  let v = VVec.make ~initialize:(fun _ -> co 1.) 1000 in
  let o = VVec.dot_product v v in
  check (float 0.1) "big" 1000. !(o.value)

let test_mapi () =
  let m = FMat.zeros 2 2 in
  let expected = [ [ (1, 1); (1, 2) ]; [ (2, 1); (2, 2) ] ] in
  let actual = FMat.mapi (fun (x, y) _ -> (x + 1, y + 1)) m in
  let actual = FMat.to_row_list actual in
  let actual = List.map FVec.to_list actual in
  Alcotest.(check' (list (list (pair int int))))
    ~msg:"mapi works correctly" ~expected ~actual

let test_transpose () =
  let m = Mat.make ~initialize:(fun x y -> (x, y)) 2 2 in
  let expected =
    Mat.make ~initialize:(fun x y -> (y, x)) 2 2
    |> Mat.to_row_list |> List.map Vec.to_list
  in
  let actual = Mat.transpose m |> Mat.to_lists in
  Alcotest.(check' (list (list (pair int int))))
    ~msg:"transpose works correctly" ~expected ~actual

let test_map_cols () =
  let r = ref 1 in
  let get_and_incr r =
    let v = !r in
    incr r;
    v
  in
  let m = Mat.make ~initialize:(fun _ _ -> get_and_incr r) 2 2 in
  let expected = [ [ 4; 6 ]; [ 4; 6 ] ] in
  let actual =
    Mat.map_cols
      (fun col ->
        let s = Vec.fold ( + ) 0 col in
        Vec.map (fun _ -> s) col)
      m
    |> Mat.to_lists
  in
  Alcotest.(check' (list (list int)))
    ~msg:"column map works correctly" ~expected ~actual

let test_mat_prod_simple () =
  let r = ref 1 in
  let get_and_incr r =
    let v = !r in
    incr r;
    v
  in
  let m1 = Mat.make ~initialize:(fun _ _ -> get_and_incr r) 2 2 in
  let m1 = Mat.map Float.of_int m1 in
  let expected = [ [ 7.; 10. ]; [ 15.; 22. ] ] in
  let actual = FMat.mult m1 m1 |> FMat.to_lists in
  Alcotest.(check' (list (list (float 0.05))))
    ~msg:"matrix multiplication works" ~expected ~actual

let test_mat_prod_dims () =
  let m1 = FMat.zeros 2 2 in
  let m2 = FMat.zeros 2 3 in
  let expected = (2, 3) in
  let m3 = FMat.mult m1 m2 in
  let actual = (m3.rows, m3.cols) in
  Alcotest.(check' (pair int int))
    ~msg:"matrix multiplication works" ~expected ~actual

let test_get_val () =
  let m = Mat.make ~initialize:(fun x y -> (x, y)) 2 2 in
  let expected = (1, 1) in
  let actual = Mat.get_val 1 1 m in
  Alcotest.(check' (pair int int)) ~msg:"get_val works on 2 2" ~expected ~actual;
  let expected = (0, 1) in
  let actual = Mat.get_val 0 1 m in

  Alcotest.(check' (pair int int))
    ~msg:"get_val works on 1 2 " ~expected ~actual

(* let twos = Mat.make ~initialize:(fun _ -> 1.) 2 2 *)

(* let test_matrix_prod () = *)
(*   twos @ twos |> check (array @@ array @@ float 0.1) "works small" *)

let tests =
  [
    ("dot product", `Quick, test_dot_product);
    ("dot product", `Quick, test_dot_product_small);
    ("dot product", `Quick, test_dot_product_big);
    ("matrices", `Quick, test_mapi);
    ("matrices", `Quick, test_transpose);
    ("matrices", `Quick, test_map_cols);
    ("matrices", `Quick, test_mat_prod_simple);
    ("matrices", `Quick, test_mat_prod_dims);
    ("matrices", `Quick, test_get_val);
  ]
