module Problem18a

    open System
    open System.Collections.Generic
    open Util

    type Source = 
        | Variable of String
        | FixedValue of int64
    
    type Instruction = 
        | Set of Source * Source
        | Add of Source * Source
        | Multiply of Source * Source
        | Modulo of Source * Source
        | Jgz of Source * Source
        | Sound of Source
        | Recover of Source
    
    type State = {  variableValues: Dictionary<String,int64>;
                    instructions: Instruction[]; 
                    mutable position: int64;
                    mutable lastPlayedSound: int64;
                    mutable continu: bool }

    let parseInstruction (line: String) = 
        let (|ParseSource|) s =
            match Int64.TryParse(s) with
            | true, i -> FixedValue(i)
            | _       -> Variable(s)

        let parts = line.Split(" \t".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
        match parts with
        |  [| "set"; ParseSource s1; ParseSource s2 |]  -> Set(s1, s2)
        |  [| "add"; ParseSource s1; ParseSource s2 |]  -> Add(s1, s2)
        |  [| "mul"; ParseSource s1; ParseSource s2 |]  -> Multiply(s1, s2)
        |  [| "mod"; ParseSource s1; ParseSource s2 |]  -> Modulo(s1, s2)
        |  [| "jgz"; ParseSource s1; ParseSource s2 |]  -> Jgz(s1, s2)
        |  [| "snd"; ParseSource source; |]  -> Sound(source)
        |  [| "rcv"; ParseSource source; |]  -> Recover(source)
        |  _ -> invalidOp "bad input string"
    
    let parseInstructions input =
        let sc = Scanner(input, false)
        sc.Lines |> Seq.map parseInstruction |> Seq.toArray

    let followInstruction state = 
        let init source = 
            match source with
            | FixedValue(_) -> ()
            | Variable(name) -> if not (state.variableValues.ContainsKey(name)) then state.variableValues.Add(name, 0L)

        let getValue source = 
            init source
            match source with
            | FixedValue(i) -> i
            | Variable(name) -> state.variableValues.[name]

        let setValue source i = 
            init source
            match source with
            | FixedValue(_) -> ()
            | Variable(name) -> state.variableValues.[name] <- i

        let instruction = state.instructions.[int state.position]
        match instruction with
        | Set(s1, s2) -> 
            setValue s1 (getValue s2)
            state.position <- state.position + 1L
        | Add(s1, s2) -> 
            setValue s1 (getValue s1 + getValue s2)
            state.position <- state.position + 1L
        | Multiply(s1, s2) -> 
            setValue s1 (getValue s1 * getValue s2)
            state.position <- state.position + 1L
        | Modulo(s1, s2) -> 
            setValue s1 (getValue s1 % getValue s2)
            state.position <- state.position + 1L
        | Jgz(source, jump) -> 
            if getValue source > 0L then
                state.position <- state.position + (getValue jump)
            else
                state.position <- state.position + 1L
        | Sound(source) -> 
            state.lastPlayedSound <- getValue source
            state.position <- state.position + 1L
        | Recover(source) -> 
            if getValue source <> 0L then state.continu <- false
            state.position <- state.position + 1L

    let rec followInstructions state = 
        if state.position < (int64 state.instructions.Length) && state.continu then 
            followInstruction state
            followInstructions state

    let solveSilver input =
        let state = {   variableValues = Dictionary();
                        instructions = parseInstructions input;
                        position = 0L;
                        continu = true;
                        lastPlayedSound = -1L; }
        state |> followInstructions
        state.lastPlayedSound

