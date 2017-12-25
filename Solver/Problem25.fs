module Problem25

    open System.Collections.Generic

    type Action = A | B | C | D | E | F

    type State = {  mutable position: int;   
                    values: Dictionary<int,int>; 
                    mutable action: Action; }

    let doStep state = 
        if not(state.values.ContainsKey(state.position)) then state.values.Add(state.position, 0)
        match state.action, state.values.[state.position] with
        | A, 0 ->   state.values.[state.position] <- 1
                    state.position <- state.position + 1
                    state.action <- B
        | A, 1 ->   state.values.[state.position] <- 0
                    state.position <- state.position - 1
                    state.action <- C
        | B, 0 ->   state.values.[state.position] <- 1
                    state.position <- state.position - 1
                    state.action <- A
        | B, 1 ->   state.values.[state.position] <- 1
                    state.position <- state.position + 1
                    state.action <- D
        | C, 0 ->   state.values.[state.position] <- 1
                    state.position <- state.position + 1
                    state.action <- A
        | C, 1 ->   state.values.[state.position] <- 0
                    state.position <- state.position - 1
                    state.action <- E
        | D, 0 ->   state.values.[state.position] <- 1
                    state.position <- state.position + 1
                    state.action <- A
        | D, 1 ->   state.values.[state.position] <- 0
                    state.position <- state.position + 1
                    state.action <- B
        | E, 0 ->   state.values.[state.position] <- 1
                    state.position <- state.position - 1
                    state.action <- F
        | E, 1 ->   state.values.[state.position] <- 1
                    state.position <- state.position - 1
                    state.action <- C
        | F, 0 ->   state.values.[state.position] <- 1
                    state.position <- state.position + 1
                    state.action <- D
        | F, 1 ->   state.values.[state.position] <- 1
                    state.position <- state.position + 1
                    state.action <- A
        | _ -> invalidOp "bad state"

    let doSteps n state = 
        for _ in 1..n do
            doStep state

    let solveSilver() =
        let state = { position = 0; action = A; values = Dictionary() }
        doSteps 12_173_597 state
        state.values.Values |> Seq.filter ((=) 1) |> Seq.length

