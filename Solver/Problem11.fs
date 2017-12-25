module Problem11

    open Util

    let (++) (x,y) (dx,dy) = (x+dx,y+dy)

    let getVector direction = 
        match direction with
        | "n" -> (0,1)
        | "s" -> (0,-1)
        | "se" -> (1,0)
        | "ne" -> (1,1)
        | "nw" -> (-1,0)
        | "sw" -> (-1,-1)
        | _ -> invalidOp "bad direction"
    
    let getDistance (x,y) = 
        let result1 = abs x + abs y
        let result2 = abs x + abs (y - x)
        let result3 = abs y + abs (x - y)
        min result1 (min result2 result3)

    let solveSilver input =
        let sc = Scanner(input, false, " \r\n\t,")
        let pos = sc.Tokens |> Seq.fold (fun p token -> p ++ getVector token) (0,0)
        getDistance pos
    
    let solveGold (input: string) =
        let sc = Scanner(input, false, " \r\n\t,")
        let _, result = 
            sc.Tokens 
            |> Seq.fold (fun (pos, maxDistance) token -> 
                            let newPos = pos ++ getVector token
                            (newPos, max maxDistance (getDistance newPos))
                         ) ((0,0),0)
        result
