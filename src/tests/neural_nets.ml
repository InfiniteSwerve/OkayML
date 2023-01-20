open Transformer_stuff
module Value = Micrograd.Value

let value_to_float (v : Micrograd.Value.t) = !(v.value)
let co = Micrograd.Value.co

let basic_neuron_test () =
  let open Neural_nets in
  Stdlib.Random.init 0;
  let neur = Neuron.init 1 in
  let x = Micrograd.Value.co 5. in
  let expected = -0.72661 in
  let actual = value_to_float (Neuron.call [ x ] neur) in
  Alcotest.(check' (float 0.001)) ~msg:"single neuron works" ~expected ~actual

let basic_layer_test () =
  let open Neural_nets in
  let module Layer = Fully_Connected_Layer in
  Stdlib.Random.init 0;
  let layer = Layer.init 2 3 in
  let x = Micrograd.Value.co 5. in
  let expected = [ -0.72661; -0.795354; -0.930156 ] in
  let actual = List.map value_to_float (Layer.call [ x ] layer) in
  Alcotest.(check' (list (float 0.001)))
    ~msg:"single layer works" ~expected ~actual

let basic_nn_test () =
  let open Neural_nets in
  Stdlib.Random.init 0;
  let n = MLP.init 3 [ 4; 4; 1 ] in
  let x = List.map (fun n -> Micrograd.Value.co n) [ 2.; 3.; -1. ] in
  let expected = [ 4; 4; 1 ] in
  let actual = List.map List.length n in
  Alcotest.(check' (list int))
    ~msg:"MLP init produces correct model sizes" ~expected ~actual;
  let expected = [ -0.774517 ] in
  let actual = List.map value_to_float (MLP.call x n) in
  Alcotest.(check' (list (float 0.001))) ~msg:"small nn works" ~expected ~actual

(* let find_gradient_of_simple_loss () = *)
(*   let open Neural_nets in *)
(*   Stdlib.Random.init 0; *)
(*   let n = MLP.init 1 [ 1 ] in *)
(*   let xs = List.map (List.map co) [ [ 1. ] ] in *)
(*   let ys = [ co 1. ] in *)

(*   let y_pred = List.map (fun input -> MLP.call input n) xs |> List.flatten in *)
(*   let loss = *)
(*     List.fold_left2 *)
(*       Value.(fun acc l r -> ((l - r) ** co 2.) + acc) *)
(*       (co 0.) ys y_pred *)
(*   in *)
(*   let (only_neuron : Neuron.t) = List.hd @@ List.hd n in *)
(*   let only_x : Value.t = List.hd @@ List.hd xs in *)
(*   let value = *)
(*     (!((List.hd @@ only_neuron.weights).value) *. !(only_x.value)) *)
(*     +. !(only_neuron.bias.value) *)
(*   in *)
(*   let expected_value = Float.tanh value in *)
(*   let expected_dldb = 1. -. (Float.tanh value ** 2.) in *)
(*   let expeceted_dldw = (1. -. (Float.tanh value ** 2.)) *. !(only_x.value) in *)
(*   let actual_value = !((List.hd y_pred).value) in *)
(*   let actual_dldb, actual_dldw = *)
(*     let params = MLP.get_parameters n in *)
(*     match List.map (fun (v : Value.t) -> !(v.grad)) with *)
(*     | [ dldb; dldw ] -> (dldb, dldw) *)
(*     | _ -> failwith "something went really wrong with this test" *)
(*   in *)

(*   assert false *)

(* let find_gradient_of_loss () = *)
(*   let open Neural_nets in *)
(*   Stdlib.Random.init 0; *)
(*   let n = MLP.init 3 [ 4; 4; 1 ] in *)
(*   let xs = *)
(*     List.map (List.map co) *)
(*       [ [ 2.; 3.; -1. ]; [ 3.; -1.; 0.5 ]; [ 0.5; 1.; 1. ]; [ 1.; 1.; -1. ] ] *)
(*   in *)
(*   let ys = List.map co [ 1.; -1.; -1.; 1. ] in *)
(*   let y_pred = List.map (fun input -> MLP.call input n) xs |> List.flatten in *)
(*   let loss = *)
(*     List.fold_left2 *)
(*       Value.(fun acc l r -> ((l - r) ** co 2.) + acc) *)
(*       (co 0.) ys y_pred *)
(*   in *)
(*   let loss_w_grad = Micrograd.Utils.backwards loss in *)
(*   (* param count: 446 *) *)
(*   let params = Micrograd.Utils.topological_sort loss_w_grad in *)
(*   Format.eprintf "param count is : %d\n%!" (List.length params); *)

(*   let expected = *)
(*     [ *)
(*       1.; *)
(*       1.; *)
(*       1.; *)
(*       1.; *)
(*       1.; *)
(*       1.; *)
(*       3.14891; *)
(*       3.54903; *)
(*       3.54903; *)
(*       -2.74879; *)
(*       -3.54903; *)
(*       -1.42005; *)
(*       -1.42005; *)
(*       -1.42005; *)
(*       1.3618; *)
(*       1.28982; *)
(*       1.28982; *)
(*       1.28982; *)
(*       1.28982; *)
(*       1.28982; *)
(*       1.28982; *)
(*       1.28982; *)
(*       1.28982; *)
(*       1.28982; *)
(*       1.28982; *)
(*       -1.42005; *)
(*       -1.42005; *)
(*       -1.16935; *)
(*       -1.15985; *)
(*       -1.15985; *)
(*       -1.15985; *)
(*       -1.15985; *)
(*       -1.15985; *)
(*       -1.15985; *)
(*       -1.15985; *)
(*       -1.15985; *)
(*       -1.15985; *)
(*       -1.15985; *)
(*       -1.42005; *)
(*       -1.42005; *)
(*       -0.121863; *)
(*       -0.0746665; *)
(*       -0.0746665; *)
(*       -0.0746665; *)
(*       -0.0746665; *)
(*       -0.0746665; *)
(*       -0.0746665; *)
(*       -0.0746665; *)
(*       -0.0746665; *)
(*       -0.0746665; *)
(*       -0.0746665; *)
(*       -1.42005; *)
(*       -1.42005; *)
(*       -0.45464; *)
(*       -0.426338; *)
(*       -0.426338; *)
(*       -0.426338; *)
(*       0.287203; *)
(*       0.00540024; *)
(*       0.00540024; *)
(*       0.00540024; *)
(*       0.00540024; *)
(*       0.00540024; *)
(*       0.00540024; *)
(*       0.00540024; *)
(*       0.00540024; *)
(*       -0.426338; *)
(*       -0.426338; *)
(*       0.784788; *)
(*       0.217901; *)
(*       0.217901; *)
(*       0.217901; *)
(*       0.217901; *)
(*       0.217901; *)
(*       0.217901; *)
(*       0.217901; *)
(*       0.217901; *)
(*       -0.426338; *)
(*       -0.426338; *)
(*       2.02919; *)
(*       0.432945; *)
(*       0.432945; *)
(*       0.432945; *)
(*       0.432945; *)
(*       0.432945; *)
(*       0.432945; *)
(*       0.432945; *)
(*       0.432945; *)
(*       -0.426338; *)
(*       -0.426338; *)
(*       -1.66091; *)
(*       -0.0481704; *)
(*       -0.0481704; *)
(*       -0.0481704; *)
(*       0.24767; *)
(*       -0.0481704; *)
(*       -0.0481704; *)
(*       0.0991165; *)
(*       -0.0481704; *)
(*       -0.0481704; *)
(*       -0.204029; *)
(*       -0.0481704; *)
(*       -0.426338; *)
(*       -1.42005; *)
(*       3.54903; *)
(*       1.; *)
(*       0.00765205; *)
(*       -0.174952; *)
(*       -0.174952; *)
(*       0.159648; *)
(*       0.174952; *)
(*       0.0292695; *)
(*       0.0292695; *)
(*       0.0292695; *)
(*       -0.0280688; *)
(*       -0.0065152; *)
(*       -0.0065152; *)
(*       -0.0065152; *)
(*       -0.0065152; *)
(*       -0.0065152; *)
(*       -0.0065152; *)
(*       -0.0065152; *)
(*       -0.0065152; *)
(*       -0.0065152; *)
(*       -0.0065152; *)
(*       0.0292695; *)
(*       0.0292695; *)
(*       0.024102; *)
(*       0.0124236; *)
(*       0.0124236; *)
(*       0.0124236; *)
(*       0.0124236; *)
(*       0.0124236; *)
(*       0.0124236; *)
(*       0.0124236; *)
(*       0.0124236; *)
(*       0.0124236; *)
(*       0.0124236; *)
(*       0.0292695; *)
(*       0.0292695; *)
(*       0.00251179; *)
(*       0.000202289; *)
(*       0.000202289; *)
(*       0.000202289; *)
(*       0.000202289; *)
(*       0.000202289; *)
(*       0.000202289; *)
(*       0.000202289; *)
(*       0.000202289; *)
(*       0.000202289; *)
(*       0.000202289; *)
(*       0.0292695; *)
(*       0.0292695; *)
(*       0.00937081; *)
(*       0.00213985; *)
(*       0.00213985; *)
(*       0.00213985; *)
(*       0.00424457; *)
(*       0.000212608; *)
(*       0.000212608; *)
(*       0.000212608; *)
(*       0.000212608; *)
(*       0.000212608; *)
(*       0.000212608; *)
(*       0.000212608; *)
(*       0.000212608; *)
(*       0.00213985; *)
(*       0.00213985; *)
(*       -0.0043852; *)
(*       -0.00126676; *)
(*       -0.00126676; *)
(*       -0.00126676; *)
(*       -0.00126676; *)
(*       -0.00126676; *)
(*       -0.00126676; *)
(*       -0.00126676; *)
(*       -0.00126676; *)
(*       0.00213985; *)
(*       0.00213985; *)
(*       -0.0163515; *)
(*       -0.00862643; *)
(*       -0.00862643; *)
(*       -0.00862643; *)
(*       -0.00862643; *)
(*       -0.00862643; *)
(*       -0.00862643; *)
(*       -0.00862643; *)
(*       -0.00862643; *)
(*       0.00213985; *)
(*       0.00213985; *)
(*       0.0102094; *)
(*       0.00102772; *)
(*       0.00102772; *)
(*       0.00102772; *)
(*       -0.00488008; *)
(*       0.00102772; *)
(*       0.00102772; *)
(*       -0.00377971; *)
(*       0.00102772; *)
(*       0.00102772; *)
(*       0.00396556; *)
(*       0.00102772; *)
(*       0.00213985; *)
(*       0.0292695; *)
(*       -0.174952; *)
(*       1.; *)
(*       0.0021026; *)
(*       -0.0917082; *)
(*       -0.0917082; *)
(*       0.087503; *)
(*       0.0917082; *)
(*       0.00821757; *)
(*       0.00821757; *)
(*       0.00821757; *)
(*       -0.00788047; *)
(*       -0.00293759; *)
(*       -0.00293759; *)
(*       -0.00293759; *)
(*       -0.00293759; *)
(*       -0.00293759; *)
(*       -0.00293759; *)
(*       -0.00293759; *)
(*       -0.00293759; *)
(*       -0.00293759; *)
(*       -0.00293759; *)
(*       0.00821757; *)
(*       0.00821757; *)
(*       0.00676678; *)
(*       0.00650342; *)
(*       0.00650342; *)
(*       0.00650342; *)
(*       0.00650342; *)
(*       0.00650342; *)
(*       0.00650342; *)
(*       0.00650342; *)
(*       0.00650342; *)
(*       0.00650342; *)
(*       0.00650342; *)
(*       0.00821757; *)
(*       0.00821757; *)
(*       0.000705199; *)
(*       0.000164562; *)
(*       0.000164562; *)
(*       0.000164562; *)
(*       0.000164562; *)
(*       0.000164562; *)
(*       0.000164562; *)
(*       0.000164562; *)
(*       0.000164562; *)
(*       0.000164562; *)
(*       0.000164562; *)
(*       0.00821757; *)
(*       0.00821757; *)
(*       0.00263091; *)
(*       0.00260347; *)
(*       0.00260347; *)
(*       0.00260347; *)
(*       0.00134184; *)
(*       0.00014125; *)
(*       0.00014125; *)
(*       0.00014125; *)
(*       0.00014125; *)
(*       0.00014125; *)
(*       0.00014125; *)
(*       0.00014125; *)
(*       0.00014125; *)
(*       0.00260347; *)
(*       0.00260347; *)
(*       -0.00220215; *)
(*       -0.00216355; *)
(*       -0.00216355; *)
(*       -0.00216355; *)
(*       -0.00216355; *)
(*       -0.00216355; *)
(*       -0.00216355; *)
(*       -0.00216355; *)
(*       -0.00216355; *)
(*       0.00260347; *)
(*       0.00260347; *)
(*       -0.00745114; *)
(*       -0.000617535; *)
(*       -0.000617535; *)
(*       -0.000617535; *)
(*       -0.000617535; *)
(*       -0.000617535; *)
(*       -0.000617535; *)
(*       -0.000617535; *)
(*       -0.000617535; *)
(*       0.00260347; *)
(*       0.00260347; *)
(*       0.00553778; *)
(*       0.00351865; *)
(*       0.00351865; *)
(*       0.00351865; *)
(*       -0.00102746; *)
(*       0.00351865; *)
(*       0.00351865; *)
(*       0.00423727; *)
(*       0.00351865; *)
(*       0.00351865; *)
(*       -0.000601976; *)
(*       0.00351865; *)
(*       0.00260347; *)
(*       0.00821757; *)
(*       -0.0917082; *)
(*       1.; *)
(*       3.32143; *)
(*       3.64496; *)
(*       3.64496; *)
(*       -2.99791; *)
(*       -3.64496; *)
(*       -1.17924; *)
(*       -2.56181; *)
(*       -1.17924; *)
(*       -1.17924; *)
(*       1.13087; *)
(*       1.10942; *)
(*       2.38979; *)
(*       1.10942; *)
(*       1.10942; *)
(*       1.71289; *)
(*       1.10942; *)
(*       1.10942; *)
(*       -0.866495; *)
(*       1.10942; *)
(*       1.10942; *)
(*       1.70368; *)
(*       1.10942; *)
(*       1.10942; *)
(*       2.14866; *)
(*       1.10942; *)
(*       -0.456721; *)
(*       -1.17924; *)
(*       -1.17924; *)
(*       -0.971048; *)
(*       -0.929738; *)
(*       -2.07066; *)
(*       -0.929738; *)
(*       -0.929738; *)
(*       -1.50304; *)
(*       -0.929738; *)
(*       -0.929738; *)
(*       0.799646; *)
(*       -0.929738; *)
(*       -0.929738; *)
(*       -1.49867; *)
(*       -0.929738; *)
(*       -0.929738; *)
(*       -1.8826; *)
(*       -0.929738; *)
(*       0.13398; *)
(*       -1.17924; *)
(*       -1.17924; *)
(*       -0.101198; *)
(*       -0.0166885; *)
(*       -0.0909882; *)
(*       -0.0166885; *)
(*       -0.0166885; *)
(*       -0.0802931; *)
(*       -0.0166885; *)
(*       -0.0166885; *)
(*       0.0601078; *)
(*       -0.0166885; *)
(*       -0.0166885; *)
(*       -0.0745997; *)
(*       -0.0166885; *)
(*       -0.0166885; *)
(*       -0.0868046; *)
(*       -0.0166885; *)
(*       1.92611; *)
(*       -1.17924; *)
(*       -1.17924; *)
(*       -0.377542; *)
(*       -0.317546; *)
(*       -0.739141; *)
(*       -0.317546; *)
(*       -0.317546; *)
(*       0.243436; *)
(*       0.204378; *)
(*       0.210132; *)
(*       0.204378; *)
(*       0.204378; *)
(*       -0.209531; *)
(*       0.204378; *)
(*       0.204378; *)
(*       0.220508; *)
(*       0.204378; *)
(*       0.204378; *)
(*       0.215887; *)
(*       0.204378; *)
(*       -0.544957; *)
(*       -0.317546; *)
(*       -0.317546; *)
(*       0.621191; *)
(*       0.593173; *)
(*       0.807644; *)
(*       0.593173; *)
(*       0.593173; *)
(*       -0.813871; *)
(*       0.593173; *)
(*       0.593173; *)
(*       1.24598; *)
(*       0.593173; *)
(*       0.593173; *)
(*       1.02409; *)
(*       0.593173; *)
(*       0.297058; *)
(*       -0.317546; *)
(*       -0.317546; *)
(*       1.73081; *)
(*       1.29289; *)
(*       1.71659; *)
(*       1.29289; *)
(*       1.29289; *)
(*       -1.73076; *)
(*       1.29289; *)
(*       1.29289; *)
(*       2.59973; *)
(*       1.29289; *)
(*       1.29289; *)
(*       2.13259; *)
(*       1.29289; *)
(*       -0.536836; *)
(*       -0.317546; *)
(*       -0.317546; *)
(*       -1.42; *)
(*       -0.540168; *)
(*       -0.583792; *)
(*       -0.540168; *)
(*       -0.540168; *)
(*       1.02176; *)
(*       0.592371; *)
(*       -0.540168; *)
(*       -0.540168; *)
(*       0.116681; *)
(*       -0.682188; *)
(*       -0.540168; *)
(*       -0.540168; *)
(*       -0.293978; *)
(*       -0.631666; *)
(*       -0.540168; *)
(*       -0.670523; *)
(*       -0.317546; *)
(*       -0.849268; *)
(*       -1.17924; *)
(*       3.64496; *)
(*     ] *)
(*   in *)

(*   let actual = List.map (fun (p : Micrograd.Value.t) -> !(p.grad)) params in *)
(*   Alcotest.(check' (list (float 0.001))) *)
(*     ~msg:"backwards produces reasonable gradients" ~expected ~actual *)

let tests =
  [
    ("neuron test", `Quick, basic_neuron_test);
    ("layer test", `Quick, basic_layer_test);
    ("nn test", `Quick, basic_nn_test);
    (* ("nn test", `Quick, find_gradient_of_loss); *)
  ]
