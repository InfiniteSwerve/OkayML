(* First index is column, second is row in column *)
type matrix
type t = matrix 
type axis = 
  | Col
  | Row
  
val map : matrix -> (float -> float) -> matrix 
val map_vector : (float -> float) -> axis -> int -> matrix -> matrix 

val make : int -> int -> ('a -> float) -> matrix 
val zeros : unit -> matrix 
val randn : unit -> matrix 

val get_line : int -> int -> matrix -> float array
val get_val : int -> int -> matrix -> float 

val add : matrix -> matrix -> matrix 
val sub : matrix -> matrix -> matrix 
val mult : matrix -> matrix -> matrix 

val std : matrix -> float 
val mean : matrix -> float 

val reshape : int -> int -> matrix -> matrix
val transpose : matrix -> matrix 

val einsum : string -> matrix -> matrix
