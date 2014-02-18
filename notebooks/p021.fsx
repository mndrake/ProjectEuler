let factorial n = {1 .. n} |> Seq.reduce (*)

let rec loop (remainder:int) (numbers:string) (candidates:Set<int>) = 
    match Set.count candidates with
    |0 -> numbers
    |k -> let n = remainder / factorial k
          candidates |> Set.toSeq
                     |> Seq.nth n
                     |> fun x -> 
                        
                        loop (remainder%(factorial k)) (numbers + string x) (candidates.Remove(x))

let remainder = 4
let numbers = "0"
let candidates = set [1;2]
let k = 2
let n = remainder / factorial k
let x = 0
candidates |> Set.toSeq |> Seq.nth 2

//loop 4 "" (set [0;1;2])
//    let k = remainder / factorial (candidates |> Set.count)
