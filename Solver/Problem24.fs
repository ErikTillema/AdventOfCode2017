module Problem24

    open Util
    open System.Collections.Generic

    type Edge = {   from: Node;
                    too: Node;
                    index: int;
                    weight: int; }
    and  Node = {   index: int; 
                    mutable edges: Edge list }
    
    let nodes = Dictionary<int,Node>()

    let getNode i = 
        if not(nodes.ContainsKey(i)) then 
            let node = { index=i; edges=[] }
            nodes.Add(i, node)
        nodes.[i]

    let parseInput input = 
        nodes.Clear()
        let parseLine edgeIndex line = 
            let sc = Scanner(line, false, " \t\r\n/")
            let from = sc.NextInt().Value |> getNode
            let too  = sc.NextInt().Value |> getNode
            let edge = { from=from; too=too; index=edgeIndex; weight=(from.index + too.index) }
            from.edges <- edge::from.edges
            too.edges <- edge::too.edges

        let sc = Scanner(input, false)
        sc.Lines |> Seq.iteri parseLine

    let getPaths start = 
        let rec getPaths' node seen (edges, length) = 
            seq {
                yield (edges, length)
                for edge in node.edges do  
                    if not (Set.contains (edge.index) seen) then
                        let too = 
                            if edge.from.index = node.index then edge.too 
                            else edge.from
                        yield! getPaths' too (Set.add (edge.index) seen) (edges+1, length+edge.weight)
            }
        getPaths' start Set.empty (0, 0)
    
    let solveSilver input =
        parseInput input
        getPaths (getNode 0) |> Seq.maxBy snd |> snd

    let solveGold input =
        parseInput input
        getPaths (getNode 0) |> Seq.max |> snd
