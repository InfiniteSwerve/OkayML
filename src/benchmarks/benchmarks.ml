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

let _ =
  Format.eprintf "\n%!";
  Format.eprintf "No references runtime: %f\n%!"
    (bench (fun () -> No_reference.run ()) 1000);
  Format.eprintf "References runtime: %f\n%!"
    (bench (fun () -> Reference.run ()) 1000)
(*
   No references runtime: 0.000550
   References runtime: 0.000001
*)
