module Problem14

    open Util
    open Problem3

    type State = { value: int array;
                   n: int;
                   mutable pos: int;
                   mutable skip: int }

    let doStep state l =  
        let n = state.n
        if l <= n then
            let vs = [ 0..l-1 ] |> List.map (fun i -> state.value.[(state.pos + i)%n]) |> List.rev
            vs |> List.indexed |> List.iter (fun (i,v) -> state.value.[(state.pos + i)%n] <- v)
            state.pos <- state.pos + l + state.skip
            state.skip <- state.skip + 1

    let initialState n = {  value = [| 0..n-1 |];
                            n = n;
                            pos = 0;
                            skip = 0 }
       
    // returns 16 values 0..255
    let getHash (input: string) =
        let lengths = input.Trim() |> Seq.map int |> Seq.toList
        let lengths = List.append lengths [ 17; 31; 73; 47; 23 ]
        let state = initialState 256
        for _ in 1..64 do
            lengths |> List.iter (doStep state)
        let sparseHash = state.value
        sparseHash |> Array.chunkBySize 16 |> Array.map (Array.fold (^^^) 0)
    
    let getUsedSquares hash = 
        let getUsedSquares' hexVal = 
            seq { 0..7 } |> Seq.map (fun i -> ((hexVal >>> (7-i)) &&& 1) = 1)
        hash |> Seq.collect getUsedSquares'
    
    let solveSilver input =
        let countUsedSquaresForRow y =
            let hash = getHash (input + "-" + (string y))
            hash |> getUsedSquares |> Seq.filter id |> Seq.length

        let n = 128
        seq { 0..n-1 } |> Seq.sumBy countUsedSquaresForRow

    let solveGold input =
        let n = 128
        let grid = Array2D.init n n (fun _ _ -> 0) // 0 = empty, -1 = used, x = group number
        
        let fillGridRow y =
            let hash = getHash (input + "-" + (string y))
            hash |> getUsedSquares |> Seq.indexed |> Seq.iter (fun (x,used) -> grid.[x,y] <- if used then -1 else 0)
        
        seq { 0..n-1 } |> Seq.iter fillGridRow

        let rec dfs x y label =
            let neighbours (x,y) = [ (x+1,y); (x-1,y); (x,y+1); (x,y-1) ]
            grid.[x,y] <- label
            for neighbour in neighbours (x,y) do
                let (nbx,nby) = neighbour
                if 0 <= nbx && nbx < n && 0 <= nby && nby < n && grid.[nbx,nby] = -1 then
                    dfs nbx nby label

        let mutable components = 0
        for x in 0..n-1 do
            for y in 0..n-1 do
                if grid.[x,y] = -1 then
                    components <- components + 1
                    dfs x y components

        components
