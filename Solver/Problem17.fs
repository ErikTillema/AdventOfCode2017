module Problem17

    type State = {  mutable buffer: int list;
                    mutable currentPos: int }

    let doStep steps state = 
        let n = state.buffer.Length
        let newPos = (state.currentPos+steps)%n
        let (h,t) = state.buffer |> List.splitAt (newPos+1)
        state.buffer <- h @ [ n ] @ t
        state.currentPos <- newPos+1

    let solveSilver steps =
        let state = { buffer = [0]; currentPos = 0 }
        for _ in 1..2017 do
            doStep steps state
        state.buffer.[(state.currentPos+1)%state.buffer.Length]

    type StateGold = {  mutable valueAfterZero: int;
                        mutable length: int;
                        mutable currentPos: int }

    let doStepGold steps (state: StateGold) = 
        let n = state.length
        let newPos = (state.currentPos+steps)%n
        if newPos = 0 then
            state.valueAfterZero <- n
        state.length <- state.length + 1
        state.currentPos <- newPos+1

    let solveGold steps =
        let state = { valueAfterZero = -1; length = 1; currentPos = 0 }
        for _ in 1..50_000_000 do
            doStepGold steps state
        state.valueAfterZero
