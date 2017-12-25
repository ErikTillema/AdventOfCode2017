module Problem6

    open Util
    open System.Collections.Generic

    type State = {  cnt: int[]; 
                    seenAfterSteps: Dictionary<int list,int>;
                    mutable steps: int }

    let followInstruction (state: State) = 
        state.seenAfterSteps.[state.cnt |> Array.toList] <- state.steps
        let n = state.cnt.Length
        let (maxi, maxv) = state.cnt |> Array.indexed |> Array.maxBy (fun (i,v) -> (v,-i)) // highest value, lowest index
        let rest = maxv%n
        let div = maxv/n
        state.cnt.[maxi] <- 0
        for i in 0..n-1 do
            state.cnt.[i] <- state.cnt.[i] + div
        for i in 0..rest-1 do
            let j = (maxi+1+i)%n
            state.cnt.[j] <- state.cnt.[j] + 1
        state.steps <- state.steps + 1

    let rec followInstructions state = 
        if not(state.seenAfterSteps.ContainsKey(state.cnt |> Array.toList)) then 
            followInstruction state
            followInstructions state

    let getStartingState input = {  cnt = input; 
                                    seenAfterSteps = Dictionary();
                                    steps = 0 }

    let solveSilver input =
        let state = getStartingState input
        followInstructions state
        state.steps

    let solveGold input =
        let state = getStartingState input
        followInstructions state
        let seenAfter = state.seenAfterSteps.[state.cnt |> Array.toList]
        state.steps - seenAfter

