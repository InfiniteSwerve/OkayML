type 'a dif = 
  | D of 'a * 'a dif Lazy.t

let rec dZero : float dif = D (0., lazy dZero)

let dConst x = D (x, lazy dZero)

let rec ( + ) (D (x, x')) (D (y, y')) = 
  D (x +. y, lazy (Lazy.force x' + Lazy.force y') )
  
let rec ( - ) (D (x, x')) (D (y, y')) = 
  D (x -. y, lazy (Lazy.force x' - Lazy.force y'))
  
let rec ( * ) (D (x0, x') as x) (D (y0, y') as y) = 
  D (x0 *. y0, lazy (x * Lazy.force y' + y * Lazy.force x'))
  
let rec ( / ) (D (x0, x') as x) (D (y0, y') as y) = 
  D (x0 /. y0, lazy ((y * Lazy.force x' - x * Lazy.force y')/ (y * y)))
  
let rec ( ** ) (D (x0, x') as x) (D (y0, _dZero) as y) = 
  D (Float.pow x0 y0, lazy (y * (x ** (y - dConst 1.)) * Lazy.force x'))
  
let  dlift f f' (D (u0, u') as u)= 
  D (f u0, lazy(f' u * Lazy.force u'))
  

