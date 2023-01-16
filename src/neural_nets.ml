open Micrograd

module Neuron = struct
  type neuron = { weights : Value.t list; bias : Value.t }
  type t = neuron

  let init in_size =
    let weights =
      List.init in_size (fun _ -> Value.co (Stdlib.Random.float 2. -. 1.))
    in
    let bias = Value.co (Stdlib.Random.float 2. -. 1.) in
    { weights; bias }

  let call input (neuron : neuron) =
    let open Value in
    List.map2 (fun (l : t) r -> l * r) neuron.weights input
    |> List.fold_left ( + ) (co 0.)
    |> fun ans -> ans + neuron.bias |> tanh
end

module Layer = struct
  type layer = Neuron.t list
  type t = layer

  let init in_size out_size = List.init out_size (fun _ -> Neuron.init in_size)
  let call input layer = List.map (Neuron.call input) layer
end

module MLP = struct
  type mlp = Layer.t list
  type t = mlp

  let init in_size out_sizes =
    let rec build network to_build =
      match to_build with
      | in_size :: out_size :: tl ->
          build (Layer.init in_size out_size :: network) (out_size :: tl)
      | _ :: [] -> List.rev network
      | _ -> failwith "mlp init failed"
    in
    build [] (in_size :: out_sizes)

  let call input network = List.map (Layer.call input) network
end
