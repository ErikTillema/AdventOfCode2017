module MathExt

    open System

    /// Returns a modulo m
    let inline modulo m a = ((a%m)+m)%m

    /// Returns x^y modulo m
    let rec powModulo m x y = 
        if y = 0 then 
            modulo m 1
        else
            let resultHalf = powModulo m x (y/2)
            let mutable result = (resultHalf |> int64) * (resultHalf |> int64) // avoid overflow
            if y%2 = 1 then result <- result * (x |> int64);
            modulo (m |> int64) result |> int

    /// Returns the multiplicative inverse of a modulo p,
    /// so returns x for which a*x = 1 modulo p.
    /// p should be prime.
    let inverseModulo p a =
        powModulo p a (p-2)

    /// Returns whether or not n is a prime number
    /// O(sqrt(n))
    let isPrime (n: int) = 
        let isDivisor i = n % i = 0
        let sqrt n = Math.Floor(Math.Sqrt (float n)) |> int
        if n <= 1 then false
        else
            // NB: using a sequence here instead of a list makes a huge performance difference for big n
            let hasDivisor = seq { 2..sqrt n } |> Seq.exists (fun i -> isDivisor i) // Seq.exists is lazy 
            not hasDivisor
