(* TODO: This wastes half the computation, 
   can we use x2 somehow *)
let rec rand_normal () = 
  Random.self_init (); 
  let u1 = Random.float 1. in 
  let u2 = Random.float 1. in 
  let v1 = 2. *. u1 -. 1. in
  let v2 = 2. *. u2 -. 1. in 
  let s = v1 ** 2. +. v2 ** 2. in
  if s >= 1. then rand_normal () 
  else 
  let x1 = v1 *. (Float.sqrt (((-.2.)*.Float.log s)/.s)) in 
  let _x2 = v2 *. (Float.sqrt (((-.2.)*.Float.log s)/.s)) in 
  x1

