module Problem12

    open Util
    open System.Collections.Generic

    type Node = {   id: int; 
                    mutable neighbours: Node list }

    let getNode =
        let nodes = Dictionary()
        (fun id ->
            if not(nodes.ContainsKey(id)) then
                let node = { id=id; neighbours=[] }
                nodes.Add(id, node) |> ignore
            nodes.[id]
        )
    
    let parseNode line = 
        let sc = Scanner(line, false, " \t\r\n<->,")
        let node = getNode (sc.NextInt().Value)
        node.neighbours <- sc.Ints |> Seq.map getNode |> Seq.toList
        node
    
    let connectedComponent node = 
        let seen = HashSet()
        let rec dfs n = 
            seen.Add(n.id) |> ignore
            for nb in n.neighbours do
                if not (seen.Contains(nb.id)) then
                    dfs nb
        dfs node
        seen |> Seq.toList |> List.map getNode
  
    let connectedComponents nodes = 
        let seen = HashSet()
        seq {
            for node in nodes do
                if not(seen.Contains(node.id)) then
                    let component = connectedComponent node
                    for n in component do
                        seen.Add(n.id) |> ignore
                    yield component
        }

    let solveSilver input =
        let sc = Scanner(input, false)
        let nodes = sc.Lines |> Seq.map parseNode |> Seq.toList
        let node = getNode 0
        node |> connectedComponent |> List.length
    
    let solveGold (input: string) =
        let sc = Scanner(input, false)
        let nodes = sc.Lines |> Seq.map parseNode |> Seq.toList
        nodes |> connectedComponents |> Seq.length
