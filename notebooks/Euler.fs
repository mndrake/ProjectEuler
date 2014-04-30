module Euler

open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

type Euler =
    /// returns execution time in seconds and the result of the function
    static member Timer (f : unit -> 'T) =
        let stopWatch = Stopwatch.StartNew()
        let result = f()
        stopWatch.Stop()
        result, stopWatch.Elapsed.TotalSeconds

    static member ReadDelimitedFile<'T>(filePath, delimiter) =
        File.ReadAllLines (filePath)
        |> Array.map (fun line -> line.Split [|delimiter|] |> Seq.cast<'T> |> Seq.toArray)



type Math =

    // reverses the digits of n
    static member Reverse (n:int) =
        let rec loop acc rem = 
            if rem = 0 then acc
            else loop (acc*10 + rem%10) (rem/10)
        loop 0 n

    // reverses the digits of n
    static member Reverse (n:int64) =
        let rec loop acc rem = 
            if rem = 0L then acc
            else loop (acc*10L + rem%10L) (rem/10L)
        loop 0L n

    static member Reverse (n:bigint) =
        // string manipulation is faster for bigint (aka bigint is slow)
        (string n).ToCharArray() 
        |> Array.rev 
        |> fun a -> System.String(a) 
        |> bigint.Parse 

    /// gives the nth prime number - int64
    static member Prime =
        let a = ResizeArray[2L]
        let grow() =
            let p0 = a.[a.Count-1]+1L
            let b = Array.create (int (min 10000000L p0)) true
            for di in a do
                let rec loop i =
                    if i < int64(b.Length) then
                        b.[int i] <- false
                        loop((int64 i) + di)
                let i0 = p0/di*di
                loop(if i0<p0 then i0+di-p0 else i0-p0)
            for i=0 to b.Length-1 do
                if b.[i] then a.Add(p0+(int64 i))
        fun n ->
            while n >= a.Count do
                grow()
            a.[n] |> int64

    /// gives the nth prime number - int
    static member Prime32 =
        let a = ResizeArray[2]
        let grow() =
            let p0 = a.[a.Count-1]+1
            let b = Array.create (min 10000000 p0) true
            for di in a do
                let rec loop i =
                    if i < b.Length then
                        b.[i] <- false
                        loop(i+di)
                let i0 = p0/di*di
                loop(if i0<p0 then i0+di-p0 else i0-p0)
            for i=0 to b.Length-1 do
                if b.[i] then a.Add(p0+i)
        fun n ->
            while n >= a.Count do
                grow()
            a.[n]

    ///gives the binomial coefficient (n k)
    static member Binomial(n:int,k:int) =
        let r = ref 1
        for d in 1 .. (min k (n-k)) do
            r := !r * (n - d + 1)
            r := !r / d
        !r

    ///gives the binomial coefficient (n k)
    static member Binomial(n:int64,k:int64) =
        let r = ref 1L
        for d in 1L .. (min k (n-k)) do
            r := !r * (n - d + 1L)
            r := !r / d
        !r

    ///gives the binomial coefficient (n k)
    static member Binomial(n:bigint,k:bigint) =
        let r = ref 1I
        for d in 1I .. (min k (n-k)) do
            r := !r * (n - d + 1I)
            r := !r / d
        !r

    static member Factorial(n:int) = [1..n]|>List.fold (*) 1
    static member Factorial(n:int64) = [1L..n]|>List.fold (*) 1L
    static member Factorial(n:bigint) = [1I..n]|>List.fold (*) 1I

    /// represents the domain of prime numbers
    static member Primes32 = Seq.initInfinite Math.Prime32

    /// represents the domain of prime numbers
    static member Primes = Seq.initInfinite Math.Prime

    /// yields true if a prime number and yields false otherwise
    static member PrimeQ (n:int64) = 
        if n<2L then 
            false
        else
            Math.Primes
            |> Seq.takeWhile (fun k -> k*k <= n)
            |> Seq.exists ( fun k -> n%k = 0L )
            |> not

    /// yields true if a prime number and yields false otherwise
    static member PrimeQ (n:int) = 
        if n<2 then 
            false
        else
            Math.Primes32
            |> Seq.takeWhile (fun k -> k*k <= n)
            |> Seq.exists ( fun k -> n%k = 0 )
            |> not

    /// gives a list of the prime factors of the integer n, together with their exponents
    static member FactorInteger (n:int64) =
        let rec loop k m factors =
            let p = Math.Prime k
            if p*p > m then 
                m::factors
                |> Seq.countBy id
                |> Seq.sortBy (fun (n,p) -> n)
                |> Seq.toList
            elif m%p=0L then loop (k) (m / p) (p::factors)
            else loop (k+1) m factors
        loop 0 n []

    /// gives a list of the prime factors of the integer n, together with their exponents
    static member FactorInteger (n:int) =
        let rec loop k m factors =
            let p = Math.Prime32 k
            if p*p > m then 
                m::factors
                |> Seq.countBy id
                |> Seq.sortBy (fun (n,p) -> n)
                |> Seq.toList
            elif m%p=0 then loop (k) (m / p) (p::factors)
            else loop (k+1) m factors
        loop 0 n []

    /// gives the number of primes less than or equal to n
    static member PrimePi (n:int64) =
        Math.Primes |> Seq.takeWhile (fun k -> k<=n) |> Seq.length

    /// gives the number of primes less than or equal to n
    static member PrimePi (n:int) =
        Math.Primes32 |> Seq.takeWhile (fun k -> k<=n) |> Seq.length

    /// integer square root
    static member ISqrt(n) = int (sqrt (float n))

    /// integer square root
    static member ISqrt(n:int64) = int64 (sqrt (float n))

    /// integer cube root
    static member ISqrt3(n) = int (float n ** (1.0/3.0) + 0.00001)

    /// integer cube root
    static member ISqrt3(n:int64) = int64 (float n ** (1.0/3.0) + 0.00001)

    static member DivisorSigma(k:int, n:int64) =
            Math.FactorInteger(n)
            |> Seq.fold (fun acc (p,n) -> acc*(k*n+1)) 1

    static member DivisorSigma(k:int, n:int) =
            Math.FactorInteger(n)
            |> Seq.fold (fun acc (p,n) -> acc*(k*n+1)) 1

//    // generates a list of all possible combinations of a given list
//    let allCombinations lst =
//        let rec comb accLst elemLst =
//            match elemLst with
//            | h::t ->
//                let next = [h]::List.map (fun el -> h::el) accLst @ accLst
//                comb next t
//            | _ -> accLst
//        comb [] lst


//    /// gives a list of the integers that divide n
//    let Divisors n =
//          n |> FactorInteger
//            |> List.collect (fun (n,p) -> [for i in 1..p -> n])
//            |> allCombinations
//            |> Seq.map (fun l -> l |> List.fold(*) 1L)
//            |> Seq.distinct
//            |> Seq.sort
//            |> fun s -> 1L::Seq.toList s
//

//


//    // generates a list of all possible combinations of a given list
//    let allCombinations lst =
//        let rec comb accLst elemLst =
//            match elemLst with
//            | h::t ->
//                let next = [h]::List.map (fun el -> h::el) accLst @ accLst
//                comb next t
//            | _ -> accLst
//        comb [] lst



//module Utils =
//
//    open System.IO
//    open System.Text.RegularExpressions
//
//    let parseFile filePath regexTest =
//        let text = File.ReadAllText (__SOURCE_DIRECTORY__ + @"\" + filePath)
//        [| for w in Regex.Matches(text, regexTest) -> w.Value |]
//
//    let getValues filePath delimiter =
//        File.ReadAllLines (__SOURCE_DIRECTORY__ + @"\" + filePath)
//        |> Array.map (fun line -> line.Split [|delimiter|] |> Array.map int)
//
//    let getLines filePath =
//        File.ReadAllLines (__SOURCE_DIRECTORY__ + @"\" + filePath)
//
//    let readMatrix filePath =
//        let lines = File.ReadAllLines (__SOURCE_DIRECTORY__ + @"\" + filePath)
//        lines |> Array.map (fun line -> (line.Split([|' '|]) |> Array.map int))
//              |> array2D
//
//    let readMatrix2 filePath delim typeFunc =
//        let lines = File.ReadAllLines (__SOURCE_DIRECTORY__ + @"\" + filePath)
//        lines |> Array.map (fun line -> (line.Split([|delim|]) 
//                                         |> Array.map typeFunc))
//              |> array2D
//
//    open System
//    open System.Text
//    open System.Collections.Generic 
//    // Save computed results by using an internal dictionary.
//    // Note that memoize is inferred to have type
//    // memoize: ('a -> 'b) -> ('a -> 'b)
//
//    let memoize f =
//
//        let cache = Dictionary<_, _>()
//        fun x ->
//            if cache.ContainsKey(x) then cache.[x]
//            else let res = f x
//                 cache.[x] <- res
//                 res


type BigRational =
  val Numerator:bigint
  val Denominator:bigint
  new(n,d) = {Numerator=n; Denominator=d}
  new(i:int) = {Numerator=(bigint i); Denominator=1I}
  new(n:int,d:int) = {Numerator=(bigint n);Denominator=(bigint d)}
  new(n:int64,d:int64) = {Numerator=(bigint n);Denominator=(bigint d)}

  interface System.IComparable with
    member x.CompareTo y = compare (x.Numerator * (y :?> BigRational).Denominator) ((y :?> BigRational).Numerator * x.Denominator)

  override x.Equals y = (x.Numerator * (y :?> BigRational).Denominator) = ((y :?> BigRational).Numerator * x.Denominator)
  override x.GetHashCode() = (box x).GetHashCode()
  override x.ToString() = sprintf "%A / %A" x.Numerator x.Denominator

  member x.Inverse = BigRational(x.Denominator, x.Numerator)

  static member Reduce (a:BigRational) = 
      bigint.GreatestCommonDivisor(a.Numerator, a.Denominator)
      |> fun m -> BigRational(a.Numerator/m, a.Denominator/m)

  static member (~-) (a:BigRational) = BigRational(-a.Numerator,a.Denominator)

  static member (+) (a:BigRational, b:BigRational) = 
      bigint.GreatestCommonDivisor(a.Numerator, a.Denominator)
      |> fun m -> BigRational(b.Denominator/m*a.Numerator+a.Denominator/m*b.Numerator, a.Denominator/m*b.Denominator)

  static member (-) (a:BigRational, b:BigRational) = a + -b

  static member (*) (a:BigRational, b:BigRational) =
      BigRational(a.Numerator*b.Numerator,a.Denominator*a.Denominator) |> BigRational.Reduce

  static member (/) (a:BigRational, b:BigRational) = 
      BigRational(a.Numerator*b.Denominator,a.Denominator*b.Numerator) |> BigRational.Reduce
  
module NumericLiteralQ =
  let FromInt32 i = BigRational(i)
  let FromZero() = BigRational(0)
  let FromOne() = BigRational(1)