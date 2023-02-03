(* TODO: This wastes half the computation,
   can we use x2 somehow *)
let rec rand_normal () =
  Random.self_init ();
  let u1 = Random.float 1. in
  let u2 = Random.float 1. in
  let v1 = (2. *. u1) -. 1. in
  let v2 = (2. *. u2) -. 1. in
  let s = (v1 ** 2.) +. (v2 ** 2.) in
  if s >= 1. then rand_normal ()
  else
    let x1 = v1 *. Float.sqrt (-2. *. Float.log s /. s) in
    let _x2 = v2 *. Float.sqrt (-2. *. Float.log s /. s) in
    x1

(* takes in a multinomial distribution and outputs
   an index corresponding to an event *)
let multinomial samples probs =
  Random.self_init ();
  (* TODO: we could use binary search here instead,
     but it would be way more complicated and only be a 2x improvement *)
  let rec find index lower_bound l i =
    match l with
    | prob :: tl -> (
        let new_bound = lower_bound +. prob in
        match new_bound >= i with
        | true -> index
        | false -> find (index + 1) new_bound tl i)
    | [] ->
        failwith
          "multinomial needs a list with multiple elements that sum to one, \
           none of which are equal to one"
  in
  List.init samples (fun _ -> find 0 0. probs (Random.float 1.))
