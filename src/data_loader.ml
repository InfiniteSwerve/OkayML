let train_labels_file = In_channel.open_bin "data/train-labels-idx1-ubyte"
let train_data_file = In_channel.open_bin "data/train-images-idx3-ubyte"
let gd () = In_channel.input_line train_labels_file
let pc c = Format.eprintf "%c%!" c

let rec repl index =
  if index = 28 then
    let _ = In_channel.(input_line stdin) in
    repl 0
  else
    let s =
      String.fold_right
        (fun c l -> (match Char.code c with 0 -> ' ' | _ -> '0') :: l)
        (Option.get @@ In_channel.really_input_string train_data_file 28)
        []
    in
    Format.eprintf "\n%!";
    List.iter pc s;
    repl (index + 1)

(* let _ = *)
(*   let _ = In_channel.really_input_string train_data_file 16 |> Option.get in *)
(*   repl 0 *)

module Makemore = struct
  let names_file = In_channel.open_bin "data/names.txt"

  let get_names () =
    let rec get out =
      match In_channel.input_line names_file with
      | Some name -> get (name :: out)
      | None -> out
    in
    get []
end

module Shakespear = struct
  let get_shakespear_file () = In_channel.open_bin "data/input.txt"
end
