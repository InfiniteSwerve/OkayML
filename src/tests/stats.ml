open Transformer_stuff

let counttable_init size =
  let counts = Hashtbl.create size in
  let _ = List.init size (fun i -> Hashtbl.add counts i 0) in
  counts

let counttable_inc hashtbl key =
  let v = Hashtbl.find hashtbl key in
  Hashtbl.replace hashtbl key (v + 1)

let actual_distribution counts : float list =
  let counts =
    Hashtbl.fold (fun index v acc -> (index, v) :: acc) counts []
    |> List.sort (fun l r -> Int.compare (fst l) (fst r))
    |> List.map snd
  in
  let sum = List.fold_left ( + ) 0 counts in
  List.map (fun i -> Float.(of_int i /. of_int sum)) counts

let print_distribution dist = List.iter (Format.eprintf "%f\n%!") dist

let test_binomial () =
  let expected = [ 0.5; 0.5 ] in
  let total_runs = 100_000 in
  let counts = counttable_init (List.length expected) in
  let rec run_lots index =
    counttable_inc counts (List.hd @@ Stats.multinomial 1 expected);
    match index = total_runs with true -> () | false -> run_lots (index + 1)
  in
  run_lots 0;
  let actual = actual_distribution counts in
  Alcotest.(check' (list (float 0.01)))
    ~msg:"multinomial generates evenly distributed values across two options"
    ~expected ~actual

let test_trinomial () =
  let module Vec = Lacaml.S.Vec in
  let expected = [ 0.3; 0.3; 0.4 ] in
  let total_runs = 100_000 in
  let counts = counttable_init (List.length expected) in
  let rec run_lots index =
    counttable_inc counts (List.hd @@ Stats.multinomial 1 expected);
    match index = total_runs with true -> () | false -> run_lots (index + 1)
  in
  run_lots 0;
  let actual = actual_distribution counts in

  Alcotest.(check' (list (float 0.05)))
    ~msg:"multinomial generates distributed values across three options"
    ~expected ~actual

let test_multinomial () =
  let module Vec = Lacaml.S.Vec in
  let expected = [ 0.15; 0.05; 0.1; 0.025; 0.675 ] in
  let total_runs = 100_000 in
  let counts = counttable_init (List.length expected) in
  let rec run_lots index =
    counttable_inc counts (List.hd @@ Stats.multinomial 1 expected);
    match index = total_runs with true -> () | false -> run_lots (index + 1)
  in
  run_lots 0;
  let actual = actual_distribution counts in

  Alcotest.(check' (list (float 0.05)))
    ~msg:"multinomial generates properly distributed values across many options"
    ~expected ~actual

let tests =
  [
    ("binomial test", `Quick, test_binomial);
    ("trinomial test", `Quick, test_trinomial);
    ("multinomial test", `Quick, test_multinomial);
  ]
