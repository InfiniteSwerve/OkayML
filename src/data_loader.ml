module Utils = struct
  let string_to_list s = String.fold_right (fun c acc -> c :: acc) s []

  let n_items n l =
    let rec build index l out =
      match index = 0 with
      | true ->
          let hd, out =
            match out with
            | hd :: tl -> (hd, tl)
            | [] -> failwith "will never reach here"
          in
          (hd, List.rev out)
      | false -> (
          match l with
          | hd :: tl -> build (index - 1) tl (hd :: out)
          | [] -> failwith "n_items cannot take a list of smaller size than n")
    in
    build n l []

  let to_tuples block_size word =
    let ln = String.length word in
    let prepend = String.make block_size '.' in
    let string = prepend ^ word ^ "." in
    let rec make_tuples index out l =
      match index = 0 with
      | true -> List.rev out
      | false -> (
          match l with
          | _ :: tl -> make_tuples (index - 1) (n_items block_size l :: out) tl
          | [] ->
              failwith "to_tuples should only call this with a block size of 0")
    in
    make_tuples ln [] (string_to_list string)
end

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
