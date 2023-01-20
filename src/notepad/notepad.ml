open Transformer_stuff
open Neural_nets
module Value = Micrograd.Value

let co = Micrograd.Value.co
let n = MLP.init 3 [ 4; 4; 1 ]

let xs =
  List.map (List.map co)
    [ [ 2.; 3.; -1. ]; [ 3.; -1.; 0.5 ]; [ 0.5; 1.; 1. ]; [ 1.; 1.; -1. ] ]

let ys = List.map (List.map co) [ [ 1. ]; [ -1. ]; [ -1. ]; [ 1. ] ]
let dataset = List.combine xs ys

module Sgd = Neural_nets.Sgd (MLP)

let _ = Sgd.iter_descend dataset n 0.1 40
