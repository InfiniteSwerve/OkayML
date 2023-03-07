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
let to_tuples = Data_loader.Utils.to_tuples
let block_size = 3
let training_data = List.map (to_tuples 3) names |> List.flatten
let ys, xs = List.split training_data

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

let itos i = Itos.find i itos

let stoi =
  let rec loop i map = function
    | char :: chars -> loop (i + 1) (Stoi.add char i map) chars
    | [] -> map
  in
  loop 0 Stoi.empty char_list

let stoi c = Stoi.find c stoi
let yenc = List.map stoi ys

(* Dataset size X block_size int values *)
let xenc = List.map (fun tuple -> List.map stoi tuple) xs |> VMat.of_lists
let c = VMat.randn 27 2
let w1 = VMat.randn 6 100
let b1 = VVec.randn 100

(* let h =  *)
(*   let intermediate =  *)
