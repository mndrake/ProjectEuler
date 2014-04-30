#I "C:/Program Files/BayardRock/IFSharp/lib"
#I "C:/Program Files/BayardRock/IFSharp/src"
#load "Euler.fs"
open Euler

let p() =
    let rec loop n = function |1->n |k->bigint.ModPow(n,loop n (k-1),100000000I)
    loop 1777I 1855

Euler.Timer(p)
