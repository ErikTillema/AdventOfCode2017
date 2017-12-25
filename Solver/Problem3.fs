module Problem3

    open Util
    open MathExt
    open ArrayExt
    open System.Collections.Generic

    let rings() = Seq.initInfinite (fun i -> 2*i+1)

    let stepsForRing ring = 
        seq {
            for i in 1..ring-2 do yield (0,1) // up
            for i in 1..ring-1 do yield (-1,0) //left
            for i in 1..ring-1 do yield (0,-1) //down
            for i in 1..ring-1 do yield (1,0) //right
            yield (1,0)
        }
    
    let steps() = rings() |> Seq.collect stepsForRing

    let (++) (x,y) (dx,dy) = (x+dx,y+dy)

    let ( **) a (x,y) = (a*x,a*y)

    let coordinates() = 
        // equivalent with getGenerator and unfold:
        //let generator = SeqExt.getGenerator (steps())
        //Seq.unfold (fun s -> Some(s, s ++ generator().Value )) (0,0)
        seq {
            let mutable pos = (0,0)
            for step in steps() do
                yield pos
                pos <- pos ++ step
        }
        
    let neighbours (x,y) = [ (x+1,y); (x+1,y+1); (x,y+1); (x-1,y+1); (x-1,y); (x-1,y-1); (x,y-1); (x+1,y-1) ]

    let values() = 
        let map = Dictionary()
        map.[(0,0)] <- 1
        let getValue (x,y) = 
            let getNeighbourValue (x,y) = 
                if map.ContainsKey(x,y) then map.[(x,y)]
                else 0
            neighbours (x,y) |> List.sumBy getNeighbourValue
        seq {
            let coordinates = coordinates() |> Seq.skip 1
            for coordinate in coordinates do
                let v = getValue coordinate
                map.[coordinate] <- v
                yield v
        }
    
    let solveSilver input = 
        let dv = [| (-1,0); (0,1); (1,0); (0,-1) |]
        let a = rings() |> Seq.find (fun a -> a*a >= input)
        let r = (a-1)/2
        let mutable pos = (r, -r)
        let mutable v = a*a
        for i in 0..3 do
            if v > input then
                let diff = min (v - input) (2*r)
                pos <- pos ++ (diff ** dv.[i])
                v <- v - diff
        let (x,y) = pos
        (abs x) + (abs y)
        // alternatively, after having typed all that code for steps and coordinates:
        //let (x,y) = coordinates() |> Seq.item (input-1)
        //(abs x) + (abs y)

    let solveGold input =
        values() |> Seq.find (fun v -> v > input)

