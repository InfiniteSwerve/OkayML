open Transformer_stuff
open Matrices
module FMat = FTensor.Mat
module FVec = FTensor.Vec
module VMat = VTensor.Mat
module VVec = VTensor.Vec

module Char_pair = struct
  type t = char * char [@@deriving ord]
end

let names = Data_loader.Makemore.get_names ()

module Gram = Map.Make (Char_pair)

let to_pairs name =
  let string = "." ^ name ^ "." in
  let ln = String.length string in
  let rec do_string curr out =
    match curr = ln with
    | true -> List.rev out
    | false -> do_string (curr + 1) ((string.[curr - 1], string.[curr]) :: out)
  in
  do_string 1 []

let name_pairs = List.map to_pairs names |> List.rev |> List.concat

let add_pair_to_map map pair =
  Gram.update pair
    (fun n -> match n with Some n -> Some (n + 1) | None -> Some 1)
    map

let pair_count = List.fold_left add_pair_to_map Gram.empty name_pairs

module Alpha = Map.Make (Char)

let alphabet = ".abcdefghijklmnopqrstuvwxyz"

module S = Set.Make (Char)

let name_string = String.concat "" ("." :: names)
let unique_chars = String.fold_left (fun s c -> S.add c s) S.empty name_string
let char_list = S.fold (fun c l -> c :: l) unique_chars [] |> List.rev

module Itos = Map.Make (Int)
module Stoi = Map.Make (Char)

let itos =
  let rec loop i map = function
    | char :: chars -> loop (i + 1) (Itos.add i char map) chars
    | [] -> map
  in
  loop 0 Itos.empty char_list

let stoi =
  let rec loop i map = function
    | char :: chars -> loop (i + 1) (Stoi.add char i map) chars
    | [] -> map
  in
  loop 0 Stoi.empty char_list

(* let bigram_counts = *)
(*   Mat.make *)
(*     ~initialize:(fun rowi coli -> *)
(*       let rc, cc = (Itos.find rowi itos, Itos.find coli itos) in *)
(*       match Gram.find_opt (rc, cc) pair_count with *)
(*       | Some n -> Float.of_int n *)
(*       | None -> 1.) *)
(*     27 27 *)

(* let probs = *)
(*   Mat.map_rows *)
(*     (fun row -> *)
(*       let sm = FVec.sum row in *)
(*       FVec.map (fun count -> count /. sm) row) *)
(*     bigram_counts *)

(* let probs_w_char char = *)
(*   let i = Stoi.find char stoi in *)
(*   FMat.row probs i *)

(* let prob_w_chars (c1, c2) = *)
(*   let i1, i2 = (Stoi.find c1 stoi, Stoi.find c2 stoi) in *)
(*   FMat.get_val i1 i2 probs *)

(* let get_next_char curr = *)
(*   let curr_index = Alpha.find curr stoi in *)
(*   let probs = FMat.row probs curr_index in *)
(*   let next_char_index = List.hd @@ Stats.multinomial 1 (FVec.to_list probs) in *)
(*   alphabet.[next_char_index] *)

(* let get_name () = *)
(*   let rec loop out curr = *)
(*     let next_char = get_next_char curr in *)
(*     let out = next_char :: out in *)
(*     if next_char = '.' then List.rev out else loop out next_char *)
(*   in *)
(*   let name_list = loop [] '.' in *)
(*   List.map Char.escaped name_list |> String.concat "" *)

(* let get_loss ((c1, c2) as gram) = *)
(*   let prob = prob_w_chars gram in *)
(*   let logprob = log prob in *)
(*   Format.eprintf "%c%c: %.4f %.4f\n%!" c1 c2 prob logprob; *)
(*   logprob *)

(* let max = 20 *)

(* let rec loop l index ll = *)
(*   match index = max with *)
(*   | true -> ll *)
(*   | false -> ( *)
(*       match l with *)
(*       | gram :: tl -> *)
(*           let ll = ll +. get_loss gram in *)
(*           loop tl (index + 1) ll *)
(*       | [] -> ll) *)

(* let _ = *)
(*   Format.eprintf "nll = %.4f\n%!" (loop name_pairs 0 0. /. Float.of_int max) *)
module Value = Micrograd.Value

let ph = Format.eprintf "%s\n%!"
let co = Value.co

let one_hot classes num =
  let v = VVec.make classes (co 0.) in
  v.(num) <- co 1.;
  v

(* let xs, ys = List.split name_pairs *)
let xsc, ysc = ([ '.'; 'e'; 'm'; 'm'; 'a' ], [ 'e'; 'm'; 'm'; 'a'; '.' ])

let xs, ys =
  ( List.map (fun c -> Stoi.find c stoi) xsc,
    List.map (fun c -> Stoi.find c stoi) ysc )

let backwards = Micrograd.Utils.backwards
let w = VMat.make ~initialize:(fun _ _ -> co (Stats.rand_normal ())) 27 27

(* This is the training data, we MUST split this up *)
let xenc : VMat.t = VVec.map (one_hot 27) (Array.of_list xs) |> VMat.of_arrays

let _ =
  Format.eprintf "xenc size is: %d %d\nw size is: %d %d\n%!" xenc.rows xenc.cols
    w.rows w.cols

(* Forward pass *)
let logits = VMat.mult xenc w
let probs = VMat.softmax logits
let likelihood = List.mapi (fun i ansi -> VMat.get_val i ansi probs) ys
let neg_log_likelihood = List.map (fun v -> Value.(neg @@ log v)) likelihood
let regularization_loss_strength = 0.01

let get_loss xs ys weights =
  let logits = VMat.mult xs weights in
  let probs = VMat.softmax logits in
  let likelihood = List.mapi (fun i ansi -> VMat.get_val i ansi probs) ys in
  let neg_log_likelihood =
    let open Value in
    List.map (fun v -> neg @@ log v) likelihood
  in
  let sm =
    let open Value in
    List.fold_left ( + ) zero neg_log_likelihood
  in
  let complexity_penalty =
    let open Value in
    let sm = VMat.fold (fun acc w -> acc + (w ** two)) zero weights in
    let dims = of_int (Int.mul weights.rows weights.cols) in
    sm / dims
  in

  let loss =
    Value.(
      (sm / co (Float.of_int @@ List.length neg_log_likelihood))
      + (co regularization_loss_strength * complexity_penalty))
  in
  loss

let _ =
  List.iteri
    (fun i (loss : Value.t) ->
      let xc, yc = (List.nth xsc i, List.nth ysc i) in
      Format.eprintf "loss for %c %c is %.4f\n%!" xc yc !(loss.value))
    neg_log_likelihood

(* Backward pass *)
let learning_rate = 0.11

let backward (loss : Value.t) weights =
  let _ = Micrograd.Utils.backwards loss in
  VMat.map
    (fun (v : Value.t) -> v.value := !(v.value) -. (!(v.grad) *. learning_rate))
    weights

let rec descend times xs ys weights =
  if times = 0 then ()
  else
    let loss = get_loss xs ys weights in
    let _ = backward loss weights in
    Format.eprintf "loss is %.4f\n%!" !(loss.value);
    descend (times - 1) xs ys weights

let _ = descend 100 xenc ys w
