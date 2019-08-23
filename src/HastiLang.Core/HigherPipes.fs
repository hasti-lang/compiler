module HigherPipes

open FParsec

let pipe6 p1 p2 p3 p4 p5 p6 f =
    pipe4 p1 p2 p3 (tuple3 p4 p5 p6)
          (fun x1 x2 x3 (x4, x5, x6) -> f x1 x2 x3 x4 x5 x6)

let pipe7 p1 p2 p3 p4 p5 p6 p7 f =
    pipe4 p1 p2 p3 (tuple4 p4 p5 p6 p7)
          (fun x1 x2 x3 (x4, x5, x6, x7) -> f x1 x2 x3 x4 x5 x6 x7)