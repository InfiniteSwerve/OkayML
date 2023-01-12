module Value = struct
  type value = {
    value : float;
    prev : prev;
    op : string;
    grad : float;
    backward : value -> value;
    label : string;
    index : int;
  }

  and prev = value list [@@deriving show]

  let eq v1 v2 : bool = v1.index = v2.index

  type t = value

  let compare a b = Int.compare a.index b.index
  let index = Atomic.make 0

  let get_index () =
    Atomic.incr index;
    Atomic.get index

  type exn = Exp_must_be_float

  let empty =
    {
      value = 0.;
      prev = [];
      op = "";
      grad = 0.;
      backward = Fun.id;
      label = "";
      index = get_index ();
    }

  let co ?(label = "") num =
    {
      value = num;
      prev = [];
      op = "c";
      grad = 0.;
      backward = Fun.id;
      label;
      index = get_index ();
    }

  let ( + ) l r =
    let value = l.value +. r.value in
    let out =
      {
        value;
        prev = [ l; r ];
        op = "+";
        grad = 0.;
        backward = empty.backward;
        label = l.label ^ "+" ^ r.label;
        index = get_index ();
      }
    in
    let backward out =
      {
        out with
        prev =
          [
            { l with grad = l.grad +. out.grad };
            { r with grad = r.grad +. out.grad };
          ];
      }
    in
    { out with backward }

  let ( * ) l r =
    let value = l.value *. r.value in
    let out =
      {
        value;
        prev = [ l; r ];
        op = "*";
        grad = 0.;
        backward = empty.backward;
        label = l.label ^ "*" ^ r.label;
        index = get_index ();
      }
    in
    let backward out =
      {
        out with
        prev =
          [
            { l with grad = l.grad +. (r.value *. out.grad) };
            { r with grad = r.grad +. (l.value *. out.grad) };
          ];
      }
    in
    { out with backward }

  let neg v = v * co (-1.)
  let ( - ) l r = l * neg r

  let exp v =
    let value = Float.exp v.value in
    let out =
      {
        value;
        prev = [ v ];
        op = "exp";
        grad = 0.;
        backward = empty.backward;
        label = "";
        index = get_index ();
      }
    in
    let backward out =
      { out with prev = [ { v with grad = out.grad +. (value *. out.grad) } ] }
    in
    { out with backward }

  (* u must be a float*)
  let ( ** ) l u =
    if u.op != "c" then failwith "exponent must be float value"
    else
      let value = l.value ** u.value in
      let out =
        {
          value;
          prev = [ l; u ];
          op = "**";
          grad = 0.;
          backward = empty.backward;
          label = "";
          index = get_index ();
        }
      in
      let backward out =
        {
          out with
          prev =
            [
              {
                l with
                grad =
                  l.grad +. (u.value *. (l.value ** (u.value -. 1.)) *. out.grad);
              };
            ];
        }
      in
      { out with backward }

  let ( / ) l r = l * (r ** co (-1.))

  let tanh input =
    let value =
      Float.((exp (2. *. input.value) -. 1.) /. (exp (2. *. input.value) +. 1.))
    in
    let out =
      {
        value;
        prev = [ input ];
        op = "tanh";
        grad = 0.;
        backward = empty.backward;
        label = "";
        index = get_index ();
      }
    in
    let backward =
      Format.eprintf "value : %f\n%!" value;
      Format.eprintf "input grad : %f\n%!" input.grad;
      Format.eprintf "((c 1.) - ((c value) ** (c 2.))) : %f\n%!"
        (co 1. - (co value ** co 2.)).value;
      Format.eprintf "out grad : %f\n%!" out.grad;
      fun out ->
        {
          out with
          prev =
            [
              {
                input with
                grad = input.grad +. ((co 1. - (out ** co 2.)).value *. out.grad);
              };
            ];
        }
    in
    { out with backward }
end

module Utils = struct
  open Value

  let topological_sort value =
    let module Values = Set.Make (Value) in
    let visited = Values.empty in
    let value = { value with grad = 1. } in
    let rec traverse ((topo_l, visited) as acc) value =
      if Values.mem value visited then acc
      else
        let visited = Values.add value visited in
        match value.prev with
        | [] -> (value :: topo_l, visited)
        | l ->
            let topo_l, visited = List.fold_left traverse (topo_l, visited) l in
            (* Format.eprintf "%s is %f with grad %f\n%!" value.label value.value value.grad;  *)
            (value :: topo_l, visited)
    in
    let out, _ = traverse ([], visited) value in
    out

  let topological_sort_backward value =
    let module Values = Set.Make (Value) in
    let visited = Values.empty in
    let value = { value with grad = 1. } in
    let rec traverse ((topo_l, visited) as acc) value =
      if Values.mem value visited then acc
      else
        let value = value.backward value in
        let visited = Values.add value visited in
        match value.prev with
        | [] -> (value :: topo_l, visited)
        | l ->
            let topo_l, visited = List.fold_left traverse (topo_l, visited) l in
            (value :: topo_l, visited)
    in
    let out, _ = traverse ([], visited) value in
    out

  let rec map f value =
    let value = f value in
    { value with prev = List.map (map f) value.prev }

  let backwards value =
    let value = { value with grad = 1. } in
    map (fun v -> v.backward v) value

  let extract_value_to_list value =
    let rec extract value out =
      let out = value :: out in
      List.fold_right (fun out value -> extract out value) value.prev out
    in
    extract value []

  let relabel label value = { value with label }

  let sort_print value =
    let grads =
      topological_sort_backward value
      |> List.map (fun value -> (value.value, value.label, value.grad))
    in
    Format.eprintf "length of chain is %d\n%!" (List.length grads);
    List.iter
      (fun (value, label, grad) ->
        Format.eprintf "%f, %s, %f\n%!" value label grad)
      grads;
    ()
end
(* TODO: Graph visualization module *)
(* TODO: We might need to make the backwards function operate on grad references
   instead of modifying the previous *)
