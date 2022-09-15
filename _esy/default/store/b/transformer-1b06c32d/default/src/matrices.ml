(* TODO: Do we really need col row info here? *)
type matrix = {
  matrix: float Array.t Array.t;
  cols : int;
  rows : int;
}
type t = matrix
type axis = 
  | Col
  | Row 

let map mi f = 
  let mo = mi.matrix in
  let mo = Array.map (fun arr ->
  Array.map f arr) mo in 
  {mi with matrix = mo}

let map_vector mi f axis index = 
  let mo = mi.matrix in
  match axis with 
  | Col -> 
    mo.(index) <- Array.map f mo.(index);
    {mi with matrix=mo}
  | Row -> 
    Array.iter (fun col -> col.(index) <- f col.(index)) mo;
    {mi with matrix=mo}

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
  map matrix initialize
  

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
  map mi (fun float -> (float -. mean_val) ** 2.)
  |> mean 
  |> (fun sum -> sum ** (0.5))

let relu vector = 
  map_vector vector (fun float ->
    match float <=0. with
    | true -> 0.
    | false -> float
  ) Col 0

