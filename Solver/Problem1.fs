module Problem1

    open Util

    let parseDigits input = 
        let sc = Scanner(input, false)
        sc.Tokens |> Seq.head |> Seq.map (int) |> Seq.map (fun a -> a - (int)'0') |> Array.ofSeq
    
    let getSum isGold (digits: int array) = 
        let n = digits.Length
        let getPartialSum2 a b = if a=b then a else 0
        let getPartialSum i = 
            if isGold then
                getPartialSum2 digits.[i] digits.[(i+n/2)%n]
            else
                getPartialSum2 digits.[i] digits.[(i+1)%n]
        [ 0..n-1 ] |> List.sumBy getPartialSum

    let solve isGold input = 
        input |> parseDigits |> getSum isGold

    let solveSilver = solve false

    let solveGold = solve true