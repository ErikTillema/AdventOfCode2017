module Problem23

    open System
    open System.Collections.Generic
    open Util
    open MathExt

    type Source = 
        | Variable of String
        | FixedValue of int64
    
    type Instruction = 
        | Set of Source * Source
        | Subtract of Source * Source
        | Multiply of Source * Source
        | Jnz of Source * Source
    
    type State = {  variableValues: Dictionary<String,int64>;
                    instructions: Instruction[]; 
                    mutable position: int64;
                    mutable multiplications: int }

    let print state = 
        let variableValues = state.variableValues |> Seq.map (fun kvp -> sprintf "(%s,%d)" (kvp.Key) (kvp.Value)) |> String.concat " "
        printfn "%d %s" (state.position) variableValues

    let parseInstruction (line: String) = 
        let (|ParseSource|) s =
            match Int64.TryParse(s) with
            | true, i -> FixedValue(i)
            | _       -> Variable(s)

        let parts = line.Split(" \t".ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
        match parts with
        |  [| "set"; ParseSource s1; ParseSource s2 |]  -> Set(s1, s2)
        |  [| "sub"; ParseSource s1; ParseSource s2 |]  -> Subtract(s1, s2)
        |  [| "mul"; ParseSource s1; ParseSource s2 |]  -> Multiply(s1, s2)
        |  [| "jnz"; ParseSource s1; ParseSource s2 |]  -> Jnz(s1, s2)
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

        match state.instructions.[int state.position] with
        | Set(s1, s2) -> 
            setValue s1 (getValue s2)
            state.position <- state.position + 1L
        | Subtract(s1, s2) -> 
            setValue s1 (getValue s1 - getValue s2)
            state.position <- state.position + 1L
        | Multiply(s1, s2) -> 
            setValue s1 (getValue s1 * getValue s2)
            state.position <- state.position + 1L
            state.multiplications <- state.multiplications + 1
        | Jnz(source, jump) -> 
            if getValue source <> 0L then
                state.position <- state.position + (getValue jump)
            else
                state.position <- state.position + 1L

    let rec followInstructions state = 
        if state.position < (int64 state.instructions.Length) then 
            print state
            followInstruction state
            followInstructions state

    let solveSilver input =
        let state = {   variableValues = Dictionary();
                        instructions = parseInstructions input;
                        position = 0L;
                        multiplications = 0; }
        state |> followInstructions
        state.multiplications

    let solveGold() =
        [106700..17..123700] |> Seq.filter (not << isPrime) |> Seq.length
    
    let solveGold_too_slow input = 
        let state = {   variableValues = Dictionary( dict([ ("a",1L); ]) ); 
                        instructions = parseInstructions input;
                        position = 0L;
                        multiplications = 0; }
        state |> followInstructions
        state.variableValues.["h"]

