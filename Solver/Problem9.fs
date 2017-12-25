module Problem9

    open Util
    open System

    type Thing = 
        | Garbage of String
        | Group of Thing list 

    let parseThing (input: char array) = 
        let mutable pos = 0

        let peekChar() = input.[pos]

        let readChar() =
            let c = input.[pos]
            pos <- pos + 1
            c
    
        let parseEscapedString() = 
            let rec parseEscapedString' acc = 
                let c = peekChar()
                if c = '>' then
                    // end of escaped string
                    acc |> List.rev |> List.toArray |> String
                elif c = '!' then
                    readChar() |> ignore // ignore escape char !
                    readChar() |> ignore // ignore escaped char
                    parseEscapedString' acc
                else
                    parseEscapedString' (readChar() :: acc)
            parseEscapedString' []

        let parseGarbage() = 
            if readChar() <> '<' then invalidOp "bad garbage"
            let s = parseEscapedString()
            if readChar() <> '>' then invalidOp "bad garbage"
            Garbage(s)
        
        let rec parseGroup() =
            if readChar() <> '{' then invalidOp "bad group"
            let children = parseThings()
            if readChar() <> '}' then invalidOp "bad group"
            Group(children)
        and parseThings() = 
            let rec parseThings' acc = 
                match parseThing'() with
                | Some(thing) -> 
                    if peekChar() = ',' then readChar() |> ignore
                    thing :: parseThings' acc
                | None -> List.rev acc
            parseThings' []
        and parseThing'() = 
            let c = peekChar()
            match c with
            | '{' -> Some(parseGroup())
            | '<' -> Some(parseGarbage())
            | _ -> None
        
        parseThing'()
    
    let rec getTotalScore offset thing = 
        match thing with 
        | Group(children) -> offset + (children |> List.sumBy (getTotalScore (offset+1)))
        | _ -> 0
    
    let rec getTotalGarbageLength thing = 
        match thing with
        | Garbage(s) -> s.Length
        | Group(children) -> children |> List.sumBy getTotalGarbageLength

    let solveSilver input =
        let thing = input |> Seq.toArray |> parseThing |> Option.get
        getTotalScore 1 thing 

    let solveGold input =
        let thing = input |> Seq.toArray |> parseThing |> Option.get
        thing |> getTotalGarbageLength
  