module Problem19

    open System
    open Util

    type Orientation = Up | Down | Left | Right

    type State = {  mutable pos: int*int;
                    mutable orientation: Orientation;
                    mutable letters: char list;
                    mutable steps: int;
                    mutable continu: bool }

    let parseGrid input = 
        let parseGridLine (line: String) = 
            line.ToCharArray()
        let sc = Scanner(input, false)
        sc.Lines |> Seq.map parseGridLine |> Seq.rev |> Seq.toArray
    
    let (++) (x,y) (dx,dy) = (x+dx,y+dy)
    
    let doMove (grid: char[][]) state = 
        let dv = function
                    | Down ->  [| (( 0,-1), Down);  ((-1,-1), Left);  (( 1,-1), Right) |]
                    | Up ->    [| (( 0, 1), Up);    ((-1, 1), Left);  (( 1, 1), Right) |]
                    | Right -> [| (( 1, 0), Right); (( 1, 1), Up);    (( 1,-1), Down)  |]
                    | Left ->  [| ((-1, 0), Left);  ((-1, 1), Up);    ((-1,-1), Down)  |]
        let getGridValue (x,y) = 
            if 0 <= x && x < grid.[0].Length && 0 <= y && y < grid.Length then
                grid.[y].[x]
            else 
                ' '
        
        let dvs = dv state.orientation
        let newPos = state.pos ++ (fst dvs.[0])
        match getGridValue newPos with 
        | '|' | '-' ->  state.pos <- newPos
                        state.steps <- state.steps + 1
        | '+' ->        let i = [1..2] |> Seq.find (fun i -> getGridValue (state.pos ++ (fst dvs.[i])) <> ' ')
                        state.pos <- newPos
                        state.orientation <- snd dvs.[i]
                        state.steps <- state.steps + 1
        | ' ' ->        state.continu <- false
        | c ->          state.pos <- newPos
                        state.letters <- List.append state.letters [c]
                        state.steps <- state.steps + 1
        
    let rec doMoves grid state = 
        if state.continu then
            doMove grid state
            doMoves grid state

    let getInitialState (grid: char[][]) = 
        let startY = grid.Length - 1
        let startX = grid.[startY] |> Array.findIndex ((=) '|')
        { pos = (startX, startY); orientation = Down; letters = []; steps = 0; continu = true }

    let solveSilver input =
        let grid = parseGrid input
        let state = getInitialState grid
        doMoves grid state
        state.letters |> Seq.toArray |> String

    let solveGold input =
        let grid = parseGrid input
        let state = getInitialState grid
        doMoves grid state
        state.steps + 1
