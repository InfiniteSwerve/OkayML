open Transformer_stuff
open Neural_nets
module Value = Micrograd.Value

let co = Micrograd.Value.co
let n = MLP.init 3 [ 4; 4; 1 ]

let xs =
  List.map (List.map co)
    [ [ 2.; 3.; -1. ]; [ 3.; -1.; 0.5 ]; [ 0.5; 1.; 1. ]; [ 1.; 1.; -1. ] ]

let ys = List.map co [ 1.; -1.; -1.; 1. ]

(* let xs = List.map (List.map co) [ [ 1. ]; [ 0.5 ]; [ 0.1 ]; [ 0.234 ] ] *)
(* let ys = List.map co (List.map (fun x -> x /. 2.) [ 1.; 0.5; 0.1; 0.234 ]) *)

let get_loss () =
  let y_pred = List.map (fun input -> MLP.call input n) xs |> List.flatten in
  List.fold_left2
    Value.(fun acc l r -> ((l - r) ** co 2.) + acc)
    (co 0.) ys y_pred

let avg_grads n =
  let params = MLP.get_parameters n in
  let grads = List.map (fun (v : Micrograd.Value.t) -> !(v.grad)) params in
  let total = List.fold_left ( +. ) 0. grads in
  total /. Float.of_int (List.length params)

let print_params n =
  let params = MLP.get_parameters n in
  let grads = List.map (fun (v : Micrograd.Value.t) -> !(v.grad)) params in
  List.iter (fun g -> Format.eprintf " %f%! " g) grads

let iter_descend network step_size count =
  Format.eprintf "\n%!";
  let rec loop index =
    if index = count then ()
    else
      let loss = get_loss () in
      (* Format.eprintf "\n%d loss is :%f\n%!" index !(loss.value); *)
      (* Format.eprintf "\naverage gradient is %f\n%!" (avg_grads network); *)
      (* print_params network; *)
      MLP.descend loss network step_size;
      loop (index + 1)
  in
  loop 0

let _ = iter_descend n 0.1 40

(* let _ = *)
(*   Format.eprintf "\n%!"; *)
(*   let l1 = get_loss () in *)
(*   Format.eprintf "Initial loss is :%f\n%!" !(l1.value); *)
(*   MLP.descend l1 n 0.005; *)
(*   let l2 = get_loss () in *)
(*   Format.eprintf "Second loss is :%f\n%!" !(l2.value) *)
