open Micrograd
(* Okay Ml*)

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

  let get_parameters neuron = neuron.bias :: neuron.weights
end

module Fully_Connected_Layer = struct
  type layer = Neuron.t list
  type t = layer

  let init in_size out_size = List.init out_size (fun _ -> Neuron.init in_size)
  let call input layer = List.map (Neuron.call input) layer

  let get_parameters layer =
    List.map Neuron.get_parameters layer |> List.flatten
end

module MLP = struct
  module Layer = Fully_Connected_Layer

  type mlp = Fully_Connected_Layer.t list
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

  let call input network = List.fold_left Layer.call input network

  let get_parameters network =
    List.map Layer.get_parameters network |> List.flatten

  let descend loss network step_size =
    let _ = Utils.backwards loss in
    let params = get_parameters network in
    List.iter
      (fun (v : Value.t) ->
        (* Format.eprintf "info for %s is:\n%!" v.op; *)
        (* Format.eprintf "initial value is %f\n%!" !(v.value); *)
        (* Format.eprintf "gradient is   %f\n%!" !(v.grad); *)
        (* Format.eprintf "step size is   %f\n%!" step_size; *)
        v.value := !(v.value) -. (!(v.grad) *. step_size))
        (* Format.eprintf "final value is %f\n%!" !(v.value)) *)
      params
end

module Sgd (Network : sig
  type t

  val get_parameters : t -> Value.t list
  val call : Value.t list -> t -> Value.t list
end) =
struct
  type network = Network.t

  let descend loss network step_size =
    let _ = Utils.backwards loss in
    let params = Network.get_parameters network in
    List.iter
      (fun (v : Value.t) ->
        (* Format.eprintf "info for %s is:\n%!" v.op; *)
        (* Format.eprintf "initial value is %f\n%!" !(v.value); *)
        (* Format.eprintf "gradient is   %f\n%!" !(v.grad); *)
        (* Format.eprintf "step size is   %f\n%!" step_size; *)
        v.value := !(v.value) -. (!(v.grad) *. step_size))
        (* Format.eprintf "final value is %f\n%!" !(v.value)) *)
      params

  (* gets loss for a single training datum *)
  let get_loss x y network =
    let y_pred = Network.call x network in
    List.fold_left2
      Value.(fun acc l r -> ((l - r) ** co 2.) + acc)
      (Value.co 0.) y y_pred

  (* What do we want iter_descend to do?
     We want it to take some dataset and do training on it.
     It'll update the gradients after batch_size trainings.
  *)
  let iter_descend dataset network step_size
      (* _batch_size _iterations *) epochs =
    Format.eprintf "\n%!";
    let rec loop index =
      if index = epochs then ()
      else
        let loss =
          List.fold_left
            Value.(fun acc (x, y) -> get_loss x y network + acc)
            (Value.co 0.) dataset
        in
        Format.eprintf "\n%d loss is :%f\n%!" index !(loss.value);
        descend loss network step_size;
        loop (index + 1)
    in
    loop 0
end
