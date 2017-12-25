module Problem22

    open Util
    open System.Collections.Generic

    type InfectionLevel = Clean | Weakened | Infected | Flagged

    type State = {  mutable position: int*int;
                    mutable orientation: int;
                    infectionLevel: Dictionary<int*int, InfectionLevel>;
                    mutable countFlippedToInfected: int; }

    let init state position = 
        if not(state.infectionLevel.ContainsKey(position)) then state.infectionLevel.Add(position, Clean)

    let getInfectionLevel state position = 
        state.infectionLevel.[position]
    
    let changeInfectionLevel isGold state position = 
        match state.infectionLevel.[position] with
        | Clean ->    state.infectionLevel.[position] <- if isGold then Weakened else Infected
        | Weakened -> state.infectionLevel.[position] <- Infected
        | Infected -> state.infectionLevel.[position] <- if isGold then Flagged else Clean
        | Flagged ->  state.infectionLevel.[position] <- Clean

    let turnRight orientation = (orientation + 1)%4
    let turnLeft orientation = (orientation + 3)%4
    let turn = function
        | Clean -> turnLeft
        | Weakened -> id
        | Infected -> turnRight
        | Flagged -> turnRight >> turnRight
    let changeOrientation state = 
        state.orientation <- turn state.infectionLevel.[state.position] state.orientation
    
    let dv = [| (0,1); // Up
                (1,0); // Right
                (0,-1); // Down
                (-1,0); // Left
             |]
    
    let (++) (x,y) (dx,dy)= (x+dx,y+dy)

    let doSteps isGold n state = 
        let doStep state = 
            // currently we do 4 lookups in the dictionary, where actually only 1 is necessary
            // so we could make this code faster.
            init state state.position
            changeOrientation state
            changeInfectionLevel isGold state state.position
            if getInfectionLevel state state.position = Infected then 
                state.countFlippedToInfected <- state.countFlippedToInfected + 1
            state.position <- state.position ++ dv.[state.orientation]
        for _ in 1..n do doStep state

    let getInitialState input = 
        let infections = Dictionary()
        let sc = Scanner(input, false)
        let grid = sc.Lines |> Seq.map (fun s -> s.ToCharArray()) |> Seq.toArray
        let h = grid.Length
        let w = grid.[0].Length
        for x in 0..w-1 do
            for y in 0..h-1 do
                if grid.[h-1-y].[x] = '#' then
                    infections.Add( (x-(w-1)/2 , y-(h-1)/2) , Infected)
        { position = (0,0); orientation = 0; infectionLevel = infections; countFlippedToInfected = 0; }

    let solveSilver (steps: int) (input: string) =
        let state = getInitialState input
        doSteps false steps state
        state.countFlippedToInfected

    let solveGold steps input =
        let state = getInitialState input
        doSteps true steps state
        state.countFlippedToInfected
