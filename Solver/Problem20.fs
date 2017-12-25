module Problem20

    open Util
    open SeqExt
    open System.IO

    type Movement = {   a : int[];
                        v0: int[]; 
                        x0: int[] }
    
    type Times = 
        | Always
        | SpecificTimes of int list
    
    type Collision = Collision of int * (int*int*int) // time * location
    
    let parseMovements input = 
        let parseMovement line = 
            match line with
            | Regex "^[ \t]*p=<(?<x0>[\-0-9]+),(?<x1>[\-0-9]+),(?<x2>[\-0-9]+)>, v=<(?<v0>[\-0-9]+),(?<v1>[\-0-9]+),(?<v2>[\-0-9]+)>, a=<(?<a0>[\-0-9]+),(?<a1>[\-0-9]+),(?<a2>[\-0-9]+)>$" 
                        [ x0;x1;x2; v0;v1;v2; a0;a1;a2 ] -> { a = [| int a0;int a1;int a2 |]; v0 = [| int v0;int v1;int v2 |]; x0 = [| int x0;int x1;int x2 |] }
            | _ -> invalidOp "bad line"
        
        let sc = Scanner(input, false)
        sc.Lines |> Seq.map parseMovement |> Seq.toArray

    let getCollisionTimes a1 a2 v1 v2 x1 x2 =
        let isInteger (t: float) = (floor t) = t
        let convertToSpecificTimes (ts: float list) = 
            SpecificTimes( ts |> List.filter isInteger |> List.map int |> List.filter (fun t -> t >= 0) )
        let a = 0.5*(float a1 - float a2)
        let b = (float v1 - float v2) + a
        let c = float x1 - float x2
        let D = b*b - 4.0*a*c
        match a, b, c, D with
        | 0., 0., 0., _ -> Always
        | 0., 0., _ , _ -> SpecificTimes([])
        | 0., _ , _ , _ -> convertToSpecificTimes [ (-c/b) ]
        | _ , _ , _ , D when D < 0. -> SpecificTimes([])
        | _ , _ , _ , 0.            -> convertToSpecificTimes [ (-b/(2.*a)) ]
        | _ , _ , _ , D when D > 0. -> convertToSpecificTimes [ ((-b - sqrt D)/(2.*a)); ((-b + sqrt D)/(2.*a)) ]
    
    let getLocation movement t = 
        let x = [|0..2|] |> Array.map (fun i -> movement.x0.[i] + movement.v0.[i] * t + movement.a.[i] * ((t * (t+1)) / 2) )
        ( x.[0], x.[1], x.[2] )

    let getCollisions movement1 movement2 =
        let folder times i = 
            let timesToMerge = getCollisionTimes movement1.a.[i] movement2.a.[i] movement1.v0.[i] movement2.v0.[i] movement1.x0.[i] movement2.x0.[i]
            match times, timesToMerge with
            | Always, _ -> timesToMerge
            | _, Always -> times
            | SpecificTimes(ts1), SpecificTimes(ts2) -> SpecificTimes( Set.intersect (ts1 |> Set.ofSeq) (ts2 |> Set.ofSeq) |> List.ofSeq )
        let times = [0..2] |> Seq.fold folder Always
        match times with
        | Always -> invalidOp "particles have exactly the same trajectories"
        | SpecificTimes(ts) -> ts |> List.map (fun t -> Collision(t, getLocation movement1 t))

    let solveSilver input =
        let movements = parseMovements input
        let getAbsA (movement: Movement) = [0..2] |> Seq.sumBy (fun i -> abs movement.a.[i])
        let getAbsVComparedToA a v = 
            match a with 
            | a when a > 0 -> v
            | a when a = 0 -> abs v
            | a when a < 0 -> -v
        let getAbsV (movement: Movement) = [0..2] |> Seq.sumBy (fun i -> getAbsVComparedToA movement.a.[i] movement.v0.[i])
        movements |> Seq.indexed |> Seq.minBy (fun (i,movement) -> (getAbsA movement, getAbsV movement)) |> fst

    let solveGold input =
        let movements = parseMovements input
        let n = movements |> Array.length
        let getUniqueCollisionIfAny i j = 
            match getCollisions movements.[i] movements.[j] with
            | [] -> None
            | [collision] -> Some(collision,[i;j])
            | _ -> invalidOp "pair of particles has multiple collisions"
        let collisions = 
            seq{0..n-1} |> getChooseCombinations 2 |> Seq.map (fun [|i;j|] -> getUniqueCollisionIfAny i j) |> Seq.filter Option.isSome |> Seq.map Option.get 
            |> Seq.groupBy fst
            |> Seq.map (fun (key,kvps) -> (key, kvps |> Seq.map snd |> Seq.collect id |> Set.ofSeq))
            |> Seq.sortBy (fun (Collision(t,_) ,_) -> t)
            |> Seq.toArray

        let hasCollided = Array.create n false
        let doCollision (collision,indexes) = 
            let countNotCollided = indexes |> Seq.filter (fun i -> not(hasCollided.[i])) |> Seq.length
            if countNotCollided > 1 then
                for i in indexes do
                    hasCollided.[i] <- true
        collisions |> Seq.iter doCollision |> ignore

        let countNotCollided = hasCollided |> Seq.filter (not) |> Seq.length
        countNotCollided
