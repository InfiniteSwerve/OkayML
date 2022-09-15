type matrix = float Array.t
type axis = 
  | Col
  | Row 

let map m f = 
  Array.map (fun arr ->
  Array.map f arr) m

let map_vector m f axis index = 
  match axis with 
  | Col -> 
    Array.map f m.(index)
  | Row -> 
    Array.map (fun col -> 
      col.(index) <- f col.(index)) m 

let make ~initialize n m = 
  let matrix = Array.make_matrix n m 0 in
  map matrix initialize
