let bench f runs =
  let rec go acc index =
    let t1 = Unix.gettimeofday () in
    f ();
    let t2 = Unix.gettimeofday () in
    let time = t2 -. t1 in
    let acc = time +. acc in
    match index = runs with true -> acc | false -> go acc (index + 1)
  in
  go 0. 0 /. Float.of_int runs

module Autodiff = struct
  let normal_addr amnt =
    List.init amnt (Fun.const 1.) |> List.fold_left ( +. ) 0.

  let normal_mult amnt =
    List.init amnt (Fun.const 1.) |> List.fold_left ( *. ) 0.

  let normal_exp amnt =
    List.init amnt (Fun.const 1.) |> List.fold_left ( ** ) 0.

  let d_addr amnt =
    let open Transformer_stuff.Autodiff in
    List.init amnt (Fun.const (dConst 1.)) |> List.fold_left ( + ) (dConst 0.)

  let d_mult amnt =
    let open Transformer_stuff.Autodiff in
    List.init amnt (Fun.const (dConst 1.)) |> List.fold_left ( * ) (dConst 0.)

  let d_exp amnt =
    let open Transformer_stuff.Autodiff in
    List.init amnt (Fun.const (dConst 1.)) |> List.fold_left ( * ) (dConst 0.)

  (* let () = *)
  (*   let stats = 15 in *)
  (*   let runs = 10000 in *)
  (*   Format.eprintf "normal add : %f\n%!" *)
  (*     (bench (fun () -> ignore @@ normal_addr runs) stats); *)
  (*   Format.eprintf "diff add : %f\n%!" *)
  (*     (bench (fun () -> ignore @@ d_addr runs) stats); *)
  (*   Format.eprintf "normal mult : %f\n%!" *)
  (*     (bench (fun () -> ignore @@ normal_mult runs) stats); *)
  (*   Format.eprintf "diff mult: %f\n%!" *)
  (*     (bench (fun () -> ignore @@ d_mult runs) stats); *)
  (*   Format.eprintf "normal exp : %f\n%!" *)
  (*     (bench (fun () -> ignore @@ normal_exp runs) stats); *)
  (*   Format.eprintf "diff exp : %f\n%!" *)
  (*     (bench (fun () -> ignore @@ d_exp runs) stats) *)
end

let max = 1_000_000

module No_reference = struct
  let a = 0

  let run () =
    let rec loop a = if a = max then () else loop (a + 1) in
    loop 0
end

module Reference = struct
  let a = ref 0

  let run () =
    let rec loop () =
      if !a = max then ()
      else (
        a := !a + 1;
        loop ())
    in
    loop ()
end

(* let _ = *)
(*   Format.eprintf "\n%!"; *)
(*   Format.eprintf "No references runtime: %f\n%!" *)
(*     (bench (fun () -> No_reference.run ()) 1000); *)
(*   Format.eprintf "References runtime: %f\n%!" *)
(*     (bench (fun () -> Reference.run ()) 1000) *)
(*
   No references runtime: 0.000550
   References runtime: 0.000001
*)

let mat_mul up_to =
  let module FMat = Transformer_stuff.Matrices.FTensor.Mat in
  let iters = Int.of_float @@ Float.log2 (Float.of_int up_to) in
  let rec loop size iters_left =
    let m = FMat.zeros size size in
    Format.eprintf "Matmul of size %dx%d takes: %f\n%!" size size
      (bench (fun () -> ignore @@ FMat.mult m m) 12);
    if iters_left = 1 then () else loop (size * 2) (iters_left - 1)
  in
  loop 1 (iters + 1)

let _ = mat_mul 1024
(*
   Matmul of size 1x1 takes: 0.000001
   Matmul of size 2x2 takes: 0.000001
   Matmul of size 4x4 takes: 0.000005
   Matmul of size 8x8 takes: 0.000033
   Matmul of size 16x16 takes: 0.000109
   Matmul of size 32x32 takes: 0.000517
   Matmul of size 64x64 takes: 0.003862
   Matmul of size 128x128 takes: 0.029885
   Matmul of size 256x256 takes: 0.254951
   Matmul of size 512x512 takes: 2.329368
   Matmul of size 1024x1024 takes: 20.618629 -> Takes pytorch 0.027 O_o
*)
