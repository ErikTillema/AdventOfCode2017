module Problem5

    open Util

    let isGold = true

    type State = {  offsets: int[]; 
                    mutable position: int;
                    mutable steps: int }

    let followInstruction isGold (state: State) = 
        let newPosition = state.position + state.offsets.[state.position]
        if isGold && state.offsets.[state.position] >= 3 then
            state.offsets.[state.position] <- state.offsets.[state.position] - 1
        else
            state.offsets.[state.position] <- state.offsets.[state.position] + 1
        state.position <- newPosition
        state.steps <- state.steps + 1

    let rec followInstructions isGold state = 
        if state.position < state.offsets.Length then 
            followInstruction isGold state
            followInstructions isGold state

    let solve isGold input =
        let sc = Scanner(input, false)
        let state = {   offsets = sc.Ints |> Seq.toArray; 
                        position = 0;
                        steps = 0 }
        followInstructions isGold state
        state.steps
                
    let solveSilver = solve false

    let solveGold = solve true
