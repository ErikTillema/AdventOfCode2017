module Problem18b

    open System
    open System.Collections.Generic
    open Util
    open System.Linq

    type Source = 
        | Variable of String
        | FixedValue of int64
    
    type Instruction = 
        | Set of Source * Source
        | Add of Source * Source
        | Multiply of Source * Source
        | Modulo of Source * Source
        | Jgz of Source * Source
        | Send of Source
        | Receive of Source
    
    type State = {  instructions: Instruction[];
                    mutable continu: bool;
                    mutable runningProgram: int;
                    isWaiting: bool[];
                    valuesSent: int[];
                    variableValues: Dictionary<String,int64>[];
                    position: int64[];
                    outgoingQueue: LinkedList<int64>[] }

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
        |  [| "snd"; ParseSource source; |]  -> Send(source)
        |  [| "rcv"; ParseSource source; |]  -> Receive(source)
        |  _ -> invalidOp "bad input string"
    
    let parseInstructions input =
        let sc = Scanner(input, false)
        sc.Lines |> Seq.map parseInstruction |> Seq.toArray

    let followInstruction state = 
        let vars = state.variableValues.[state.runningProgram]
        let init source = 
            match source with
            | FixedValue(_) -> ()
            | Variable(name) -> if not (vars.ContainsKey(name)) then vars.Add(name, 0L)

        let getValue source = 
            init source
            match source with
            | FixedValue(i) -> i
            | Variable(name) -> vars.[name]

        let setValue source i = 
            init source
            match source with
            | FixedValue(_) -> ()
            | Variable(name) -> vars.[name] <- i

        let instruction = state.instructions.[int state.position.[state.runningProgram]]
        match instruction with
        | Set(s1, s2) -> 
            setValue s1 (getValue s2)
            state.position.[state.runningProgram] <- state.position.[state.runningProgram] + 1L
        | Add(s1, s2) -> 
            setValue s1 (getValue s1 + getValue s2)
            state.position.[state.runningProgram] <- state.position.[state.runningProgram] + 1L
        | Multiply(s1, s2) -> 
            setValue s1 (getValue s1 * getValue s2)
            state.position.[state.runningProgram] <- state.position.[state.runningProgram] + 1L
        | Modulo(s1, s2) -> 
            setValue s1 (getValue s1 % getValue s2)
            state.position.[state.runningProgram] <- state.position.[state.runningProgram] + 1L
        | Jgz(source, jump) -> 
            if getValue source > 0L then
                state.position.[state.runningProgram] <- state.position.[state.runningProgram] + (getValue jump)
            else
                state.position.[state.runningProgram] <- state.position.[state.runningProgram] + 1L
        | Send(source) -> 
            state.outgoingQueue.[state.runningProgram].AddLast(getValue source) |> ignore
            state.valuesSent.[state.runningProgram] <- state.valuesSent.[state.runningProgram] + 1
            state.position.[state.runningProgram] <- state.position.[state.runningProgram] + 1L
        | Receive(source) -> 
            let queueIsEmpty = state.outgoingQueue.[state.runningProgram].Count = 0
            let otherQueueIsEmpty = state.outgoingQueue.[1-state.runningProgram].Count = 0
            let otherIsWaiting = state.isWaiting.[1 - state.runningProgram]
            match queueIsEmpty, otherQueueIsEmpty, otherIsWaiting with
            | _, false, _ ->   // still messages to read from queue
                setValue source (state.outgoingQueue.[1-state.runningProgram].First.Value)
                state.outgoingQueue.[1-state.runningProgram].RemoveFirst()
                state.position.[state.runningProgram] <- state.position.[state.runningProgram] + 1L
            | _, _, false ->    // other program wasn't waiting, continue with other program
                state.isWaiting.[state.runningProgram] <- true
                state.runningProgram <- 1 - state.runningProgram
            | false, _, true ->  // other program was waiting but we sent messages, continue with other program
                state.isWaiting.[state.runningProgram] <- true
                state.isWaiting.[1 - state.runningProgram] <- false
                state.runningProgram <- 1 - state.runningProgram
            | true, _, true ->  // STOP! ... maybe only if no messages sent
                state.isWaiting.[state.runningProgram] <- true
                state.continu <- false

    let rec followInstructions state = 
        if state.position.[state.runningProgram] < (int64 state.instructions.Length) && state.continu then 
            followInstruction state
            followInstructions state

    let solveGold input =
        let state = {   instructions = parseInstructions input;
                        continu = true;
                        runningProgram = 0;
                        isWaiting = [| false; false |];
                        valuesSent = [| 0; 0 |];
                        variableValues = [| Dictionary(dict [("p",0L)]); 
                                            Dictionary(dict [("p",1L)]); |];
                        position = [| 0L; 0L|];
                        outgoingQueue = [| LinkedList(); LinkedList() |] }
        state |> followInstructions
        state.valuesSent.[1]

