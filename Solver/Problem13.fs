module Problem13

    open Util

    let isCaughtAt d r delay = 
        (d+delay)%(2*(r-1))=0
    
    let isCaught scanners delay =
        scanners |> Array.exists (fun (d,r) -> isCaughtAt d r delay)

    let solveSilver input =
        let sc = Scanner(input, false, " \t\r\n:")
        let scanners = sc.Ints |> Seq.chunkBySize 2 |> Seq.map (fun [|d;r|] -> (d,r)) |> Seq.toArray
        scanners |> Seq.filter (fun (d,r) -> isCaughtAt d r 0) |> Seq.sumBy (fun (d,r) -> d*r) 

    // Brute force
    // @@@ but we could do it more elegantly?
    // we can cross off delays periodicly for every scanner: 
    // scanner (d,r) is at zero at the times t = n * period, where period = (2*(r-1))
    // the package is at zero (for the layer of that scanner) at time t = d + delay
    // so we want d + delay <> n * period
    // or d + delay <> 0 (mod period)
    // or delay <> - d (mod period)
    // so for many scanners i, we have delay <> -d_i (mod period_i)
    let solveGold (input: string) =
        let sc = Scanner(input, false, " \t\r\n:")
        let scanners = sc.Ints |> Seq.chunkBySize 2 |> Seq.map (fun [|d;r|] -> (d,r)) |> Seq.toArray
        Seq.initInfinite id |> Seq.find (fun i -> not (isCaught scanners i))