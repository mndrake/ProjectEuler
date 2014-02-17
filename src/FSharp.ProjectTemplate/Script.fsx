//// prime number generator
//let rec sieve = function
//    | (p::xs) -> p :: sieve [ for x in xs do if x % p > 0 then yield x ]
//    | []      -> []
//let primes = sieve [2..1000000]
////printfn "%A" primes 


let n = 10
let primes n =
    let p = Array.create n true

    let rec loop k =
        if k*k<n then
            if p.[k-1] then
                for i = 2 to n/k do
                    p.[i*k-1] <- false
            loop (k+1)      
         
    loop 2
    [ for i = 0 to p.Length-1 do
        if p.[i] then yield i+1 ]
