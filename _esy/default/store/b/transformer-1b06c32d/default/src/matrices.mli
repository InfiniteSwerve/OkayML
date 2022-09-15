(* First index is column, second is row in column *)
type matrix
type t = matrix 
type axis = 
  | Col
  | Row
  
(* O(n) *)
val map : matrix -> (float -> float) -> matrix 
(* O(n) *)
val map_vector : t -> (float -> float) -> axis -> int -> matrix 

val make : initialize:(float -> float) -> int -> int -> matrix 
val zeros : int -> int -> matrix 
val randn : int -> int -> matrix 

val get_vector : axis -> int -> matrix -> float array
val get_val : int -> int -> matrix -> float 

val dot_product : float array -> float array -> float


val ( + ) : matrix -> matrix -> matrix 
val ( - ) : matrix -> matrix -> matrix 
val ( @ ) : matrix -> matrix -> matrix 

val sum : matrix -> float
val mean : matrix -> float 
val std : matrix -> float 
val relu : matrix -> matrix

(* val reshape : int -> int -> matrix -> matrix *)
(* val transpose : matrix -> matrix  *)


(* val einsum : string -> matrix -> matrix *)
