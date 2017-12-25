module Problem10

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
       
    let solveSilver n input =
        let state = initialState n
        input |> List.iter (doStep state)
        state.value.[0] * state.value.[1]
    
    let solveGold (input: string) =
        let lengths = input.Trim() |> Seq.map int |> Seq.toList
        let lengths = List.append lengths [ 17; 31; 73; 47; 23 ]
        let state = initialState 256
        for _ in 1..64 do
            lengths |> List.iter (doStep state)
        let sparseHash = state.value
        let denseHash = sparseHash |> Array.chunkBySize 16 |> Array.map (Array.fold (^^^) 0)
        let result = denseHash |> Array.map (sprintf "%02x") |> String.concat ""
        result