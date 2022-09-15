(* First index is column, second is row in column *)
type 'a matrix
type 'a t = 'a matrix 
type 'a vector
val equal : 'a matrix -> 'a matrix -> ('a -> 'a -> bool) -> bool

type axis = 
  | Col
  | Row
  
(* O(n) *)
val map : ('a -> 'b) -> 'a matrix -> 'b matrix 
(* O(n) *)
val map_vector : ('a -> 'a) -> axis -> int -> 'a matrix -> 'a matrix 

val fold : ( 'a -> 'b -> 'a) -> 'a -> 'b matrix -> 'a

val make : initialize:(float -> 'a) -> int -> int -> 'a matrix 
val zeros : int -> int -> float matrix 
val randn : int -> int -> float matrix 

val get_vector : axis -> int -> 'a matrix -> 'a vector
val get_val : int -> int -> 'a matrix -> 'a

val dot_product : float array -> float array -> float


val ( + ) : float matrix -> float matrix -> float matrix 
val ( - ) : float matrix -> float matrix -> float matrix 
val ( @ ) : float matrix -> float matrix -> float matrix 

val sum : float matrix -> float
val mean : float matrix -> float 
val std : float matrix -> float 
val relu : float matrix -> float matrix
val softmax : float matrix -> float matrix

(* val reshape : int -> int -> float matrix -> matrix *)
(* val transpose : float matrix -> matrix  *)


(* val einsum : string -> float matrix -> matrix *)
