module Vec = struct
  type 'a vector = 'a Array.t
  type 'a t = 'a vector

  let make size x = Array.make size x
  let map f vec = Array.map f vec
  let mapi f vec = Array.mapi f vec
  let map2 f vec1 vec2 = Array.map2 f vec1 vec2
  let fold f acc vec = Array.fold_left f acc vec
  let iter f vec = Array.iter f vec
  let of_list l = Array.of_list l
  let to_list v = Array.to_list v
  let length vec = Array.length vec
end

module Mat = struct
  (* TODO: Do we really need col row info here? *)
  type 'a matrix = { matrix : 'a Array.t Array.t; cols : int; rows : int }
  and 'a t = 'a matrix

  (* let of_row_list l = Array.of_list l *)
  let to_row_list (m : 'a matrix) : 'a Vec.t list = Array.to_list m.matrix

  let of_row_list (l : 'a Vec.t list) : 'a matrix =
    let row_count = List.length l in
    let col_count = List.hd l |> Array.length in
    { matrix = Array.of_list l; cols = col_count; rows = row_count }

  let to_lists mi = to_row_list mi |> List.map Vec.to_list

  let of_arrays (a : 'a Array.t Array.t) : 'a matrix =
    let rows = Array.length a in
    let cols = Array.length a.(0) in
    { matrix = a; cols; rows }

  let iteri f mi =
    let mo = mi.matrix in
    Array.iteri
      (fun row_index row ->
        Array.iteri (fun col_index item -> f (row_index, col_index) item) row)
      mo

  (* x11 -> x1j -> xj1 -> xij *)
  let map f mi =
    let mo = mi.matrix in
    let mo = Array.map (fun arr -> Array.map f arr) mo in
    { mi with matrix = mo }

  let mapi (f : int * int -> 'a -> 'b) mi =
    let mo = mi.matrix in
    let mo =
      Array.mapi
        (fun row_index row ->
          Array.mapi (fun col_index item -> f (row_index, col_index) item) row)
        mo
    in
    { mi with matrix = mo }

  let combine mi1 mi2 =
    let m1o = mi1.matrix in
    let m2o = mi2.matrix in
    let combo =
      Array.combine m1o m2o |> Array.map (fun (m1, m2) -> Array.combine m1 m2)
    in
    { mi1 with matrix = combo }

  (* x11 -> x1j -> xj1 -> xij *)
  let fold (f : 'a -> 'b -> 'a) acc (mi : 'b matrix) =
    let mo = mi.matrix in
    Array.fold_left (fun acc row -> Array.fold_left f acc row) acc mo

  let make ~initialize m n =
    let matrix = { matrix = Array.make_matrix m n 0; rows = m; cols = n } in
    mapi (fun (ri, ci) _ -> initialize ri ci) matrix

  let transpose mi =
    let mo = mi.matrix in
    make ~initialize:(fun coli rowi -> mo.(rowi).(coli)) mi.cols mi.rows

  let map_rows f mi =
    let mo = mi.matrix in
    let mo = Array.map f mo in
    { mi with matrix = mo }

  let map_cols f mi =
    let mi = transpose mi in
    let mi = map_rows f mi in
    transpose mi

  let equal m1i m2i equal : bool =
    if not (m1i.rows = m2i.rows && m1i.cols = m2i.cols) then false
    else
      let combo = combine m1i m2i in
      let is_eq acc (x1, x2) =
        match acc with true -> equal x1 x2 | false -> false
      in
      fold is_eq true combo

  (* TODO: Make get_row as well *)

  let row mi index : 'a Vec.t = mi.matrix.(index)
  let col mi index : 'a Vec.t = Array.map (fun row -> row.(index)) mi.matrix
  let map_row f mi = Array.map f mi.matrix
  let get_val row_index col_index mi = mi.matrix.(row_index).(col_index)
end

module Tensor (A : sig
  type t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ** ) : t -> t -> t
  val exp : t -> t
  val zero : t
  val incr : t -> t
  val of_float : float -> t
  val of_int : int -> t
end) =
struct
  let one = A.(incr zero)
  let two = A.(incr @@ incr zero)

  module Vec = struct
    open A
    include Vec

    type vector = A.t Vec.t
    type t = vector

    let dot_product v1 v2 = map2 (fun a b -> a * b) v1 v2 |> fold ( + ) zero
    let sum vec = fold ( + ) zero vec

    (* TODO: GADTs for matrix vs vector? *)
    let relu vector =
      map
        (fun float -> match float <= zero with true -> zero | false -> float)
        vector

    let softmax vector =
      let exp_mean = fold (fun acc float -> (two ** float) + acc) zero vector in
      map (fun float -> (two ** float) / exp_mean) vector
  end

  module Mat = struct
    open A
    include Mat

    type mat = A.t Mat.t
    type t = mat

    let sum mi =
      let mo = mi.matrix in
      Array.fold_left
        (fun acc col -> Array.fold_left (fun acc float -> acc + float) acc col)
        zero mo

    let zeros m n = make ~initialize:(fun _ _ -> zero) m n

    let randn m n =
      make ~initialize:(fun _ _ -> of_float @@ Stats.rand_normal ()) m n

    let add m1i m2i =
      let combo = combine m1i m2i in
      map (fun (x, y) -> x + y) combo

    let sub m1i m2i =
      let combo = combine m1i m2i in
      map (fun (x, y) -> x - y) combo

    let outer_product v1 v2 =
      let rowl = Vec.length v1 in
      let coll = Vec.length v2 in
      make ~initialize:(fun rowi coli -> v1.(rowi) *. v2.(coli)) rowl coll

    (* left vector matrix multiplication *)
    let vmult v mi =
      let vo = Vec.make (Vec.length v) zero in
      Vec.mapi (fun ind _ -> Vec.dot_product v (col mi ind)) vo

    (* right matrix vector multiplication *)
    let multv mi v =
      let vo = Vec.make (Vec.length v) zero in
      Vec.mapi (fun ind _ -> Vec.dot_product v (row mi ind)) vo

    (* TODO: This is a naive and slow O(n^3) implementation *)
    let mult m1i m2i =
      match m1i.cols = m2i.rows with
      | false ->
          failwith
            (Format.sprintf
               "inner dims of mult must be equal, instead got %d and %d"
               m1i.cols m2i.rows)
      | true ->
          let m = zeros m1i.rows m2i.cols in
          mapi (fun (ri, ci) _ -> Vec.dot_product (row m1i ri) (col m2i ci)) m

    let mean mi = sum mi / of_int (Int.mul mi.rows mi.cols)

    let std mi =
      let mean_val = mean mi in
      map (fun float -> (float - mean_val) ** two) mi |> mean |> fun sum ->
      sum ** of_float 0.5

    (* Row-wise softmax *)
    let softmax mi =
      let expd = map (fun v -> A.exp v) mi in
      Mat.map_rows
        (fun row ->
          let s = Vec.sum row in
          Vec.map (fun f -> f / s) row)
        expd

    let ( + ) = add
    let ( - ) = sub
    let ( @ ) = mult
  end
end

module Fl = struct
  open Float

  type t = Float.t

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( ** ) = ( ** )
  let exp = Float.exp
  let zero = 0.
  let incr n = n + 1.
  let decr n = n - 1.
  let of_float = Fun.id
  let to_float = Fun.id
  let of_int = of_int
  let to_int = to_int
end

module Vl = struct
  include Micrograd.Value

  type t = Micrograd.Value.t

  let incr v = v + co 1.
  let decr v = v - co 1.
  let of_float fl = co fl
  let to_float v = !(v.value)
  let of_int i = co (Int.to_float i)
  let to_int v = Int.of_float !(v.value)
end

module FTensor = Tensor (Fl)
module VTensor = Tensor (Vl)
