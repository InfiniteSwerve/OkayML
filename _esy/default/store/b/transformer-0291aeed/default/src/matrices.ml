(* TODO: Do we really need col row info here? *)
type 'a matrix = {
  matrix: 'a Array.t Array.t;
  cols : int;
  rows : int;
}
type 'a t = 'a matrix
(* TODO: GADTs for matrix vs vector? *)
type 'a vector = 'a Array.t
type axis = 
  | Col
  | Row 

let map f mi = 
  let mo = mi.matrix in
  let mo = Array.map (fun arr ->
  Array.map f arr) mo in 
  {mi with matrix = mo}

let map_vector f axis index mi = 
  let mo = mi.matrix in
  match axis with 
  | Col -> 
    mo.(index) <- Array.map f mo.(index);
    {mi with matrix=mo}
  | Row -> 
    Array.iter (fun col -> col.(index) <- f col.(index)) mo;
    {mi with matrix=mo}

let fold f acc mi = 
  let mo = mi.matrix in 
  Array.fold_left (fun acc col -> 
    Array.fold_left f acc col) acc mo 

let fold_vector f acc axis index mi = 
  let mo = mi.matrix in 
  match axis with 
  | Col -> 
    Array.fold_left f acc mo.(index)
  | Row -> 
    Array.map (fun col -> col.(index)) mo
    |> Array.fold_left f acc 

let equal m1i m2i equal=
  if not (m1i.rows = m2i.rows && m1i.cols = m2i.cols) then
    false
  else 
  let m1o = m1i.matrix in 
  let m2o = m2i.matrix in 
  (* matrix with two cols per col *)
  let combo = Array.combine m1o m2o in 
  (* matrix with bools if col els are eq*)
  let equal_mat = Array.map (fun (col1, col2) -> 
    Array.map2 (equal)
    col1 col2 
    ) combo in 
  (* if one is false, all is false *)
  let bool_fold (acc: bool) bool =
    match acc with 
    | true -> bool
    | false -> false 
  in 
  fold (bool_fold : bool -> bool -> bool )
    true {m1i with matrix=equal_mat}

let sum mi = 
  let mo = mi.matrix in 
  Array.fold_left (fun acc col -> 
  Array.fold_left (fun acc float -> acc +. float) acc col) 0. mo
let make ~initialize m n  = 
  let matrix = {
    matrix = Array.make_matrix m n 0. ; 
    cols = m ;
    rows = n ;
  }
  in
  map initialize matrix
  

let zeros m n = 
  make ~initialize:Fun.id m n 

let randn m n  = 
  make ~initialize:(fun _ ->
  Stats.rand_normal ())
  m n 
  
let get_vector axis index mi = 
  let mo = mi.matrix in
  match axis with 
  | Col -> mo.(index)
  | Row -> 
    Array.map (fun col -> col.(index)) mo

let get_val col_index row_index mi =
  let mo = mi.matrix in
  mo.(col_index).(row_index)
  
  
let dot_product v1 v2 = 
  let v1l = Array.length v1 - 1 in 
  let v2l = Array.length v2 - 1 in 
  match v1l = v2l with
  | true -> 
  let rec go i acc = 
    match i < 0 with 
      | true -> acc
      | false ->
        let res = v1.(i) *. v2.(i) in
        go (i-1) (acc +. res)
    in 
    go v1l 0. 
  | false -> failwith (Format.sprintf "length of v1 must equal length of v2, but %d != %d" v1l v2l) 

let add m1i m2i = 
  let m1o = m1i.matrix in
  let m2o = m2i.matrix in
  let m3o = Array.map2 (fun c1 c2 ->
  Array.map2 (fun v1 v2 -> v1 +. v2) c1 c2)
  m1o m2o in
  {m1i with matrix=m3o}

let sub m1i m2i = 
  let m1o = m1i.matrix in
  let m2o = m2i.matrix in
  let m3o = Array.map2 (fun c1 c2 ->
  Array.map2 (fun v1 v2 -> v1 -. v2) c1 c2)
  m1o m2o in
  {m1i with matrix=m3o}

let mult_naive m1i m2i = 
  let m1o = m1i.matrix in 
  let m2o = m2i.matrix in 
  if m1i.cols = m2i.rows then 
    let m3o = zeros m1i.cols m2i.rows in 
    for i=0 to m2i.cols - 1 do
      for j=0 to m1i.rows - 1 do
        m3o.matrix.(i).(j) <- dot_product m1o.(j) m2o.(i);
      done
    done;
  m3o
  else failwith (Format.sprintf "cols of m1 must equal rows of m2, but %d != %d" m1i.cols m2i.rows)

  
let ( + ) = add
let ( - ) = sub
let ( @ ) = mult_naive

let mean mi = 
  (sum mi) /. (Float.of_int (mi.rows * mi.cols))
  

let std mi = 
  let mean_val = mean mi in
  map (fun float -> (float -. mean_val) ** 2.) mi
  |> mean 
  |> (fun sum -> sum ** (0.5))

let relu vector = 
  map_vector (fun float ->
    match float <=0. with
    | true -> 0.
    | false -> float
  ) Col 0 vector 

let softmax vector = 
  let exp_mean = fold_vector (fun acc float -> 2. ** float +. acc) 0. Col 0 vector in 
  map_vector (fun float -> 2. ** float /. exp_mean) Col 0 vector 
