module Problem2

    open Util
    open SeqExt

    let getValue isGold values = 
        if isGold then
            let divides [| a; b |] = 
                if b%a = 0 then
                    Some (b/a)
                elif a%b = 0 then
                    Some (a/b)
                else
                    None
            getChooseCombinations 2 values |> Seq.map divides |> Seq.filter Option.isSome |> Seq.head |> Option.get
        else
            (values |> Array.max) - (values |> Array.min)

    let getChecksum isGold line = 
        let sc = Scanner(line, false)
        sc.Ints |> Seq.toArray |> getValue isGold
    
    let solve isGold input =
        let sc = Scanner(input, false)
        sc.Lines |> Seq.sumBy (getChecksum isGold)
        
    let solveSilver = solve false

    let solveGold = solve true