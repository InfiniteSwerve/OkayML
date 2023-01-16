type 'a node = { value : int ref; prev : 'a list }

let a = { value = ref 1; prev = [] }
let b = { value = ref 0; prev = [ a ] }
let c = { value = ref 0; prev = [ a ] }

let _ =
  a.value := 2;
  Format.eprintf "b's a is : %i\n%!" !((List.hd b.prev).value)
