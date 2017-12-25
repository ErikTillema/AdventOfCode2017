module Problem8

    open Util
    open System
    open System.Collections.Generic

    type Source = 
        | Variable of String
        | FixedValue of int
    
    type Condition =
        | Condition of Source * (int -> int -> bool) * Source
    
    type Action = 
        | Increment of Source * Source
        | Decrement of Source * Source
    
    type Instruction = 
        | Instruction of Action * Condition
    
    type State = {  variableValues: Dictionary<String,int>;
                    mutable highestValue: int; }
    
    let getValue (state: State) source =
        match source with
        | FixedValue(v) -> v
        | Variable(name) ->
            if not(state.variableValues.ContainsKey(name)) then
                state.variableValues.[name] <- 0
            state.variableValues.[name]
      
    let updateValue (state: State) source newValue = 
        match source with
        | FixedValue(v) -> invalidOp "can't update value of a fixed value"
        | Variable(name) ->
            state.variableValues.[name] <- newValue
            state.highestValue <- max (state.highestValue) newValue

    let parseInstruction (line: String) = 
        let (|ParseSource|) s =
            match Int32.TryParse(s) with
            | true, i -> FixedValue(i)
            | _       -> Variable(s)
        
        let (|ParseOperator|) s =
            match s with 
            | ">=" -> (>=)
            | ">" -> (>)
            | "<=" -> (<=)
            | "<" -> (<)
            | "==" -> (=)
            | "!=" -> (<>)
            | _ -> invalidOp "bad operator"
        
        let (|ParseCondition|) tokens =
            match tokens with 
            | [ ParseSource conditionSource1; ParseOperator operator; ParseSource conditionSource2 ] -> Condition(conditionSource1, operator, conditionSource2)
            | _ -> invalidOp "bad condition"

        let (|ParseAction|) tokens = 
            match tokens with
            | [ ParseSource source; "inc"; ParseSource deltaSource ] -> Increment(source, deltaSource)
            | [ ParseSource source; "dec"; ParseSource deltaSource ] -> Decrement(source, deltaSource)
            | _ -> invalidOp "bad action"
        
        let (|ParseInstruction|) s = 
            let sc = Scanner(s, false)
            let tokens = sc.Tokens |> Seq.toList
            match tokens |> List.splitAt 3 with
            | (ParseAction action, "if" :: (ParseCondition condition)) -> Instruction(action, condition)
            | _ -> invalidOp "bad line"
        
        match line with
        | ParseInstruction instruction -> instruction
    
    let parseInstructions input =
        let sc = Scanner(input, false)
        sc.Lines |> Seq.map parseInstruction |> Seq.toArray
    
    let evaluate state (Condition(s1, operator, s2)) = 
        operator (getValue state s1) (getValue state s2)
    
    let doAction state action =
        match action with
        | Increment(s1, s2) -> 
            let newValue = (getValue state s1) + (getValue state s2)
            updateValue state s1 newValue
        | Decrement(s1, s2) -> 
            let newValue = (getValue state s1) - (getValue state s2)
            updateValue state s1 newValue
    
    let followInstruction state (Instruction(action, condition)) = 
        if evaluate state condition then
            doAction state action
    
    let solve isGold input =
        let instructions = parseInstructions input
        let state = {   variableValues = Dictionary();
                        highestValue = 0 }
        instructions |> Seq.iter (followInstruction state)
        if isGold then
            state.highestValue
        else
            state.variableValues.Values |> Seq.max

    let solveSilver = solve false

    let solveGold = solve true
  