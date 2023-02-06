(*
   What does this library do:

   This library will tracks the gradients of any expression created with its interface. 
   You can propagate the gradients backwards from the highest level expression i.e. c = a + b => backwards c
*)
module Value = struct
  type value = {
    value : float ref;
    prev : prev;
    op : string;
    grad : float ref;
    backward : value -> unit;
    label : string;
    index : int;
  }

  and prev = value list

  type t = value

  let eq v1 v2 : bool = v1.index = v2.index

  (* get value *)
  let gv v = !(v.value)

  (* get grad *)
  let gg v = !(v.grad)
  let compare a b = Int.compare a.index b.index
  let index = Atomic.make 0

  let get_index () =
    Atomic.incr index;
    Atomic.get index

  type exn = Exp_must_be_float

  let empty =
    {
      value = ref 0.;
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
      value = ref num;
      prev = [];
      op = "c";
      grad = ref 0.;
      backward = (fun _ -> ());
      label;
      index = get_index ();
    }

  let zero = co ~label:"0" 0.
  let one = co ~label:"1" 1.
  let two = co ~label:"2" 2.

  let ( + ) l r =
    let value = ref @@ (gv l +. gv r) in
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
      let og = gg out in
      l.grad := gg l +. og;
      r.grad := gg r +. og
    in
    { out with backward }

  let ( * ) l r =
    let value = ref @@ (gv l *. gv r) in
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
      let og = gg out in
      l.grad := gg l +. (gv r *. og);
      r.grad := gg r +. (gv l *. og)
    in
    { out with backward }

  let neg v = v * co (-1.)
  let ( - ) l r = l + neg r

  let exp v =
    let value = ref @@ Float.exp (gv v) in
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
    let backward out = v.grad := gg v +. (!value *. gg out) in
    { out with backward }

  (* u must be a float*)
  let ( ** ) l u =
    if u.op != "c" then failwith "exponent must be float value"
    else
      let value = ref @@ (gv l ** gv u) in
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
        let og = gg out in
        l.grad := gg l +. (gv u *. ((gv l ** (gv u -. 1.)) *. og));
        u.grad := gg u +. ((gv l ** gv u) *. og)
      in
      { out with backward }

  let ( / ) l r = l * (r ** co (-1.))

  let tanh input =
    let value =
      ref
      @@ Float.((exp (2. *. gv input) -. 1.) /. (exp (2. *. gv input) +. 1.))
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
      input.grad := gg input +. (gv (co 1. - (out ** co 2.)) *. gg out)
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
    let values = topological_sort value in
    List.iter (fun v -> v.grad := 0.) values;
    let value = { value with grad = ref 1. } in
    List.iter (fun v -> v.backward v) (topological_sort value);
    value

  let relabel label value = { value with label }
end
