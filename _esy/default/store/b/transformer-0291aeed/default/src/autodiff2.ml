module Value = struct
  type 'a value = 
    | Value of 'a
  type 'a t = 'a value
  
  let ( + ) (Value a) (Value b) = 
    Value (a +. b)

  let ( * ) (Value a) (Value b) = 
    Value (a *. b)
end

let () = 
  let open Value in 
  let a = Value 1. in
  let b = Value 1. in
  let c = Value 1. in
  let ans = a + b * c
