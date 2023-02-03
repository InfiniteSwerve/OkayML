open Matrices

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
    | true -> out
    | false -> do_string (curr + 1) ((string.[curr - 1], string.[curr]) :: out)
  in
  do_string 1 []

let name_pairs = List.map to_pairs names |> List.concat

let add_pair_to_map map pair =
  Gram.update pair
    (fun n -> match n with Some n -> Some (n + 1) | None -> Some 1)
    map

let pair_count = List.fold_left add_pair_to_map Gram.empty name_pairs

module Alpha = Map.Make (Char)

let alphabet = ".abcdefghijklmnopqrstuvwxyz"

let stoi =
  let stoi, _ =
    String.fold_left
      (fun (alpha, index) char -> (Alpha.add char index alpha, index + 1))
      (Alpha.empty, 0) alphabet
  in
  stoi

let bigram_counts = Matrices.Mat.zeros 27 27

let add_bigram_to_map n ((l, r) : char * char) =
  let row, col = (Alpha.find l stoi + 1, Alpha.find r stoi + 1) in
  n.{row, col} <- n.{row, col} +. 1.

(* let _ = List.iter (add_bigram_to_map bigram_counts) name_pairs *)

let probs =
  List.map
    (fun col -> Vec.map (fun count -> count /. Vec.sum col) col)
    (Mat.to_row_list bigram_counts)
  |> Mat.of_row_list

let get_next_char curr =
  let curr_index = Alpha.find curr stoi in
  let counts = Mat.row bigram_counts (curr_index + 1) in
  let sum = Vec.sum counts in
  let probs = Vec.map (fun c -> c /. sum) counts in
  let next_char_index = List.hd @@ Stats.multinomial 1 (Vec.to_list probs) in
  alphabet.[next_char_index]

module Bigram = struct
  let get_name () =
    let rec loop out curr =
      let next_char = get_next_char curr in
      let out = next_char :: out in
      if next_char = '.' then List.rev out else loop out next_char
    in
    let name_list = loop [] '.' in
    List.map Char.escaped name_list |> String.concat ""
end

(* Format.eprintf "\n%a" *)
(*   (Lacaml.Io.pp_lfmat *)
(*      ~vertical_context:(Some (Lacaml.Io.Context.create 2)) *)
(*      ~horizontal_context:(Some (Lacaml.Io.Context.create 3)) *)
(*      ~print_right:false ~print_foot:false ()) *)
(*   bigram_counts *)
