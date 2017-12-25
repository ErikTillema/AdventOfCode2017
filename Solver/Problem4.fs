module Problem4

    open Util
    open System

    let sortChars (s: String) = 
        s |> Seq.sort |> Array.ofSeq |> String
    
    let isValid isGold line = 
        let sc = Scanner(line, false)
        let f = 
            if isGold then sortChars
            else id
        let tokens = sc.Tokens |> Seq.map f |> Seq.toList
        let distinctTokens = tokens |> Set.ofSeq
        distinctTokens.Count = tokens.Length

    let solve isGold input =
        let sc = Scanner(input, false)
        sc.Lines |> Seq.filter (isValid isGold) |> Seq.length
        
    let solveSilver = solve false

    let solveGold = solve true