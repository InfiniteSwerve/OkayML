module Value = struct
  type value = 
    {
      value : float;
      prev: prev;
      op : string;
      grad : float;
      backward : unit -> value;
      label : string; 
      index : int 
    }
  and prev =  value list 

  type t =  value 
  
  let compare a b = 
    Int.compare a.index b.index
  
  let index = Atomic.make 0
  
  let get_index () = 
    Atomic.incr index;
    Atomic.get index

  type exn =
    | Exp_must_be_float

  let rec empty v = {value=v; prev=[]; op=""; grad=0.; backward=(fun () -> empty v); label=""; index=get_index ()}

  let c ?(label="") num  = {value=num; prev=[]; op="c"; grad=0.; backward=(empty num).backward; label; index=get_index ()}
  
  let relabel label value =
    {value with label}
  
  let ( + ) l r = 
    let value = l.value +. r.value in 
    let out = {value ; prev = [l;r]; op = "+"; grad = 0.; backward = (empty value).backward; label = l.label ^ r.label; index=get_index ()} in 
    let backward = 
      fun () -> {out with prev = [{l with grad = l.grad +. 1.0 *. out.grad}; {r with grad = r.grad +. 1.0 *. out.grad }]} in 
    {out with backward}
    
  let ( * ) l r = 
    let value = l.value *. r.value in 
    let out = {value; prev = [l;r]; op = "*"; grad = 0.; backward = (empty value).backward; label = l.label ^ r.label; index=get_index ()} in  
    let backward = 
      fun () -> {out with prev = [{l with grad = l.grad +. r.grad *. out.grad }; {r with grad = r.grad +. l.grad *. out.grad }]} in 
    {out with backward}
    
  let neg v = 
    v * (c (-1.))
    
  let ( - ) l r =
    l * (neg r)

  let exp v = 
    let value = Float.exp v.value in
    let out = {value; prev = [v]; op = "exp"; grad = 0.; backward = (empty value).backward; label = ""; index = get_index ()} in 
    let backward = 
      fun () -> {out with prev = [{v with grad = out.grad +. value *. out.grad}]} in 
    {out with backward}

    (* u must be a float*)
  let ( ** ) l u = 
    if u.op != "c" then failwith "exponent must be float value"
    else 
    let value = l.value ** u.value in 
    let out = {value; prev = [l;u]; op = "**"; grad = 0.; backward = (empty value).backward; label = ""; index = get_index ()} in 
    let backward = 
      fun () -> {out with prev = [{l with grad = l.grad +. u.value *. l.value ** (u.value -. 1.) *. out.grad }]} in 
    {out with backward}

  let ( / ) l r = 
    l * (r ** (c (-1.)))

  let tanh input = 
    let value = Float.(((exp (2. *. input.value)) -. 1.) /. ((exp (2. *. input.value)) +. 1.) ) in 
    let out = {value; prev = [input]; op = "tanh"; grad = 0.; backward = (empty value).backward; label=""; index=get_index ()} in 
    let backward = 
      fun () -> {out with prev = [{input with grad = out.grad +. ((c 1.) - (c value) ** (c 2.)).value *. out.grad}]} in 
    {out with backward}
    
end

let topological_sort value = 
  let module Values = Set.Make(Value) in 
  let visited = Values.empty in 
  let rec traverse ((topo_l, visited) as acc) value = 
    if Values.mem value visited then acc else 
    let visited = Values.add value visited in 
    match value.prev with 
    | [] -> (value::topo_l, visited)
    | l -> let (topo_l, visited) = List.fold_left traverse (topo_l, visited) l in 
      (value::topo_l, visited)
   in 
  let (out, _) = traverse ([], visited) value in 
  out

(* TODO: Graph visualization module *)
(* TODO: We might need to make the backwards function operate on grad references 
  instead of modifying the previous *)
  