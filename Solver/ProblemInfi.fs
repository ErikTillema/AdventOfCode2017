module ProblemInfi

    open Util
    open SeqExt
    open System.Collections.Generic

    let (++) (x,y) (dx,dy) = (x+dx, y+dy)

    let isMeetingPoint (positions: 'a array) = 
        let set = positions |> Set.ofSeq
        positions.Length > set.Count

    let draw points = 
        let minx = points |> List.map fst |> List.min
        let maxx = points |> List.map fst |> List.max
        let miny = points |> List.map snd |> List.min
        let maxy = points |> List.map snd |> List.max
        let set = HashSet(points)
        for y in miny .. maxy do
            for x in minx .. maxx do
                if set.Contains(x,y) then
                    printf "#"
                else
                    printf " "
            printfn ""

    let solve isGold input =
        let n = 
            let sc = Scanner(input, false)
            sc.Next().Value |> Array.ofSeq |> Array.filter ((=) '[') |> Array.length

        let startingPoints = 
            let sc = Scanner(input, false, "()[],")
            List.init n (fun _ -> (sc.NextInt().Value, sc.NextInt().Value))
    
        let moves = 
            let sc = Scanner(input, false, "()[],")
            sc.Tokens |> Seq.take (n*2) |> Seq.toList |> ignore

            // use unfold, because recursive seq leads to StackOverflowException
            let generator() = 
                match sc.NextInt(), sc.NextInt() with
                | Some(dx), Some(dy) -> Some (dx,dy)
                | _ -> None
            unfold2 generator
    
        let moveBundles = 
            moves |> Seq.chunkBySize n

        let positions = startingPoints |> Array.ofList
        let meetingPoints = HashSet()

        for moveBundle in moveBundles do
            moveBundle |> Array.iteri (fun i move -> positions.[i] <- positions.[i] ++ move)
            if isMeetingPoint positions then 
                meetingPoints.Add(positions.[0]) |> ignore
        
        if isGold then
            meetingPoints |> Seq.toList |> draw
            0
        else 
            meetingPoints.Count
    
    let solveSilver = solve false

    let solveGold = solve true