(*
   What does this library do:

   This library will tracks the gradients of any expression created with its interface. 
   You can propagate the gradients backwards from the highest level expression i.e. c = a + b => backwards c
*)
module Value = struct
  type value = {
    value : float;
    prev : prev;
    op : string;
    grad : float ref;
    backward : value -> unit;
    label : string;
    index : int;
  }

  and prev = value list

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
      grad = ref 0.;
      backward = (fun _ -> ());
      label = "";
      index = get_index ();
    }

  let co ?(label = "") num =
    let label = if label = "" then string_of_float num else label in
    {
      value = num;
      prev = [];
      op = "c";
      grad = ref 0.;
      backward = (fun _ -> ());
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
        grad = ref 0.;
        backward = empty.backward;
        label = "(" ^ l.label ^ "+" ^ r.label ^ ")";
        index = get_index ();
      }
    in
    let backward out =
      let og = !(out.grad) in
      l.grad := !(l.grad) +. og;
      r.grad := !(r.grad) +. og
    in
    { out with backward }

  let ( * ) l r =
    let value = l.value *. r.value in
    let out =
      {
        value;
        prev = [ l; r ];
        op = "*";
        grad = ref 0.;
        backward = empty.backward;
        label = "(" ^ l.label ^ "*" ^ r.label ^ ")";
        index = get_index ();
      }
    in
    let backward out =
      let og = !(out.grad) in
      l.grad := !(l.grad) +. (r.value *. og);
      r.grad := !(r.grad) +. (l.value *. og)
    in
    { out with backward }

  let neg v = v * co (-1.)
  let ( - ) l r = l + neg r

  let exp v =
    let value = Float.exp v.value in
    let out =
      {
        value;
        prev = [ v ];
        op = "exp";
        grad = ref 0.;
        backward = empty.backward;
        label = "exp (" ^ v.label ^ ")";
        index = get_index ();
      }
    in
    let backward out = v.grad := !(v.grad) +. (value *. !(out.grad)) in
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
          grad = ref 0.;
          backward = empty.backward;
          label = "(" ^ l.label ^ "**" ^ u.label ^ ")";
          index = get_index ();
        }
      in
      let backward out =
        let og = !(out.grad) in
        l.grad := !(l.grad) +. (u.value *. ((l.value ** u.value) -. 1.) *. og);
        u.grad := !(u.grad) +. ((l.value ** u.value) *. og)
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
        grad = ref 0.;
        backward = empty.backward;
        label = "";
        index = get_index ();
      }
    in
    let backward out =
      input.grad :=
        !(input.grad) +. ((co 1. - (out ** co 2.)).value *. !(out.grad))
    in
    { out with backward }
end

module Utils = struct
  open Value

  let topological_sort value =
    let module Values = Set.Make (Value) in
    let visited = Values.empty in
    let rec traverse ((topo_l, visited) as acc) value =
      if Values.mem value visited then acc
      else
        let visited = Values.add value visited in
        match value.prev with
        | [] -> (value :: topo_l, visited)
        | l ->
            let topo_l, visited = List.fold_left traverse (topo_l, visited) l in
            (value :: topo_l, visited)
    in
    let out, _ = traverse ([], visited) value in
    out

  let backwards value =
    let value = { value with grad = ref 1. } in
    List.iter (fun v -> v.backward v) (topological_sort value);
    value

  let topological_sort_backward value =
    let module Values = Set.Make (Value) in
    let visited = Values.empty in
    let value = { value with grad = ref 1. } in
    let rec traverse ((topo_l, visited) as acc) value =
      if Values.mem value visited then acc
      else (
        value.backward value;
        let visited = Values.add value visited in
        match value.prev with
        | [] -> (value :: topo_l, visited)
        | l ->
            let topo_l, visited = List.fold_left traverse (topo_l, visited) l in
            (value :: topo_l, visited))
    in
    let out, _ = traverse ([], visited) value in
    out

  let relabel label value = { value with label }
end
