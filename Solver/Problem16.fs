// For the Gold star problem:
// instead of doing all the moves every time, it would be much faster to translate the dance into
// a permutation, and perform the permutation from that moment on, instead of doing the whole dance.
// Then we could analyze the permutation: if we apply the permutation multiple times, which cycles appear?
// A number of cycles with lengths between 1 and 16 will come out.
// The state after 1 billion dances is easy to calculate: for every cycle, the state is after one billion
// dances is the same as the state after 1 billion mod cycleLength.
// Apparently in mine there are cycles of 
// 30 = 2*3*5, so probably three cycles: 6 5 and 5 (or 2,3,5,6), giving a smallest common multiple of 30.
module Problem16

    open Util
    open System
    open System.Collections.Generic

    type Move = 
        | Spin of int // length
        | Exchange of int * int
        | Partner of char * char
    
    type State = {  values: char array; 
                    seen: Dictionary<string,int>; 
                    mutable movesDone: int }

    let parseMove token = 
        match token with
        | Regex "^s(?<length>\d+)$" [ length ] -> Spin(int length)
        | Regex "^x(?<a>\d+)/(?<b>\d+)$" [ a; b ] -> Exchange(int a, int b)
        | Regex "^p(?<a>\w)/(?<b>\w)$" [ a; b ] -> Partner(char a, char b)
        | _ -> invalidOp "bad token"
    
    let parseMoves input = 
        let sc = Scanner(input, false, ",")
        sc.Tokens |> Seq.map parseMove |> Seq.toList
 
    let doMove (values: char array) move = 
        let n = values.Length
        let find a = 
            values |> Array.findIndex ((=) a)
        let swap a b = 
            let tmp = values.[a]
            values.[a] <- values.[b]
            values.[b] <- tmp
        match move with
        | Spin(l) -> 
            let newValues = Seq.append (values |> Seq.skip (n-l) |> Seq.take l) (values |> Seq.take (n-l)) |> Seq.toArray
            for i in 0..n-1 do values.[i] <- newValues.[i]
        | Exchange(a,b) -> swap a b
        | Partner(a,b) -> swap (find a) (find b)
    
    let doDanceOnce moves (state: State) = 
        moves |> Seq.iter (doMove state.values)

    let rec doDanceUntilLoopFound moves (state: State) = 
        let s = state.values |> String
        if state.seen.ContainsKey(s) then
            (state.movesDone, state.seen.[s])
        else
            state.seen.Add(s, state.movesDone)
            state.movesDone <- state.movesDone + 1
            doDanceOnce moves state
            doDanceUntilLoopFound moves state

    let solveSilver n input =
        let state = {   values = Array.init n (fun i -> 'a' + (char i));
                        seen = Dictionary();
                        movesDone = 0 }
        let moves = input |> parseMoves
        doDanceOnce moves state
        state.values |> String

    let solveGold n input =
        let state = {   values = Array.init n (fun i -> 'a' + (char i));
                        seen = Dictionary();
                        movesDone = 0 }
        let moves  = input |> parseMoves
        let N = 1_000_000_000
        let (loopStart,loopEnd) = doDanceUntilLoopFound moves state // finds loop loopStart -> loopEnd
        let loopLength = loopEnd - loopStart
        let movesToDo = (N - loopStart)%loopLength
        for _ in 1..movesToDo do
            doDanceOnce moves state
        state.values |> String

