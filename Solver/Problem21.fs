module Problem21

    open Util
    open System
    open System.Collections.Generic
    open SeqExt

    type Grid = Grid of bool[,]

    type Transformation = Transformation of Grid * Grid

    let getSize (Grid(grid1)) = grid1.GetLength(0)

    let coordinates n = cartesianProduct (seq{0..n-1}) (seq{0..n-1})

    let (|ParseGrid|) s = 
        let sc = Scanner(s, false, " \r\n\t/")
        let tokens = sc.Tokens |> Seq.map (fun s -> s.ToCharArray()) |> Seq.toArray
        let n = tokens.Length
        let result = Array2D.init n n (fun x y -> tokens.[y].[x] = '#')
        Grid(result)

    let parseTransformation (line: String) =
        match line.Split([|" => "|], StringSplitOptions.None) with
        | [| ParseGrid from; ParseGrid too |] -> Transformation(from, too)
        | _ -> invalidOp "bad line"
    
    let parseTransformations input =
        let sc = Scanner(input, false)
        sc.Lines |> Seq.map parseTransformation |> Seq.toList |> List.groupBy (fun (Transformation(from,_)) -> getSize from) |> Map.ofSeq
    
    let isIsomorph grid1 grid2 = 
        let n = getSize grid1
        let rotateOnce (x,y) = (y,n-1-x)
        let flip (x,y) = (x,n-1-y)
        let rotations = [ id; (rotateOnce); (rotateOnce >> rotateOnce); (rotateOnce >> rotateOnce >> rotateOnce) ]
        let flips = [ id; flip ]
        let rotationsAndFlips = cartesianProduct rotations flips |> Seq.map (fun (f1, f2) -> f1 >> f2)
        let isEqual (Grid(grid1)) (Grid(grid2)) map = coordinates n |> Seq.forall (fun (x,y) -> let (x2,y2) = map(x,y)
                                                                                                grid1.[x,y] = grid2.[x2,y2] )
        rotationsAndFlips |> Seq.exists ( fun f -> isEqual grid1 grid2 f)

    let getSubgrid (Grid(grid)) size (xs,ys) = 
        let result = Array2D.create size size false
        for x in 0..size-1 do
            for y in 0..size-1 do
                result.[x,y] <- grid.[xs+x,ys+y]
        Grid(result)
    
    let solveSilver steps input =
        let transformations = parseTransformations input

        let transform = 
            let cache = Dictionary<int*int,Grid>()
            (fun (Grid(grid)) size (xs,ys) ->
                let gridHash = coordinates size |> Seq.indexed |> Seq.sumBy (fun (i,(dx,dy)) -> if grid.[xs+dx,ys+dy] then (1<<<i) else 0)
                match cache.ContainsKey((size,gridHash)) with
                | true -> cache.[(size,gridHash)]
                | false -> 
                    let subGrid = getSubgrid (Grid(grid)) size (xs,ys)
                    let (Transformation(_,too)) = (Map.find size transformations) |> List.find (fun (Transformation(from,_)) -> isIsomorph from subGrid)
                    cache.Add( (size,gridHash), too)
                    too
            )
    
        let growOnce grid = 
            let size = getSize grid
            let (oldSmallSize, newSmallSize) = 
                if size%2=0 then (2,3) 
                else (3,4)
            let n = size/oldSmallSize
            let result = Array2D.create (n*newSmallSize) (n*newSmallSize) false
            for xs in 0..n-1 do
                for ys in 0..n-1 do
                    let (Grid(g)) = transform grid oldSmallSize (xs*oldSmallSize,ys*oldSmallSize)
                    for x in 0..newSmallSize-1 do
                        for y in 0..newSmallSize-1 do
                            result.[xs*newSmallSize + x, ys*newSmallSize + y] <- g.[x,y]
            Grid(result)
    
        let rec grow n grid = 
            match n with
            | 0 -> grid
            | _ -> grow (n-1) (growOnce grid)

        let initialGrid = (|ParseGrid|) ".#./..#/###"
        let (Grid(result)) = grow steps initialGrid
        let size = getSize (Grid(result))
        coordinates size |> Seq.filter (fun (x,y) -> result.[x,y]) |> Seq.length

