module Problem7

    open Util
    open System
    open System.Collections.Generic

    type Node = {  name: String;
                   mutable weight: int;
                   mutable children: Node array;
                   mutable parent: Node option;
                   mutable totalWeight: int }
    
    let getNode = 
        let nodes = Dictionary<String, Node>()
        (fun name ->
            if not(nodes.ContainsKey(name)) then 
                let node = { name = name;
                             weight = -1;
                             children = Array.empty; 
                             parent = None;
                             totalWeight = -1 }
                nodes.Add(name, node) |> ignore
            nodes.[name]
        )
    
    let rec getTotalWeight node = 
        match node.totalWeight with
        | -1 -> 
            let tw = node.weight + (node.children |> Array.sumBy getTotalWeight)
            node.totalWeight <- tw
            tw
        | tw -> tw
    
    /// Returns whether there is unbalance in the given node.
    /// If there is, it will return the child that is off in weight, and what the weight of that child should be to restore the balance.
    let getUnbalance node = 
        let childrenTotalWeights = node.children 
                                    |> Array.mapi (fun i child -> (i, getTotalWeight child)) 
                                    |> Array.groupBy (fun (i,tw) -> tw) // group by total weight
                                    |> Array.sortBy (fun (k,values) -> values.Length) // set smallest group (of one) at front
        
        if childrenTotalWeights.Length = 1 then 
            None // all same total weight
        else
            let oddOne = childrenTotalWeights.[0]
            let indexOddOne = oddOne |> snd |> Array.head |> fst
            let twOddOne = oddOne |> fst
            let twOthers = childrenTotalWeights.[1] |> fst
            let correctedWeight = (node.children.[indexOddOne].weight) + (twOthers - twOddOne)
            Some(node.children.[indexOddOne], correctedWeight)

    let parseNode line = 
        let sc = Scanner(line, false, " \t()->,")
        let name = sc.Next().Value
        let weight = sc.NextInt().Value
        let node = getNode name
        node.weight <- weight
        node.children <- sc.Tokens |> Seq.map getNode |> Seq.toArray
        node.children |> Array.iter (fun child -> child.parent <- Some(node))
        node
    
    let parseNodes input = 
        let sc = Scanner(input, false)
        sc.Lines |> Seq.map parseNode |> Seq.toList
    
    let getRoot input = 
        let nodes = parseNodes input
        let anyNode = nodes.Head
        let rec getRoot' node =
            match node.parent with
            | None -> node
            | Some(parent) -> getRoot' parent
        anyNode |> getRoot'
    
    let solveSilver input =
        let root = input |> getRoot 
        root.name

    let solveGold input =
        let root = input |> getRoot
        let rec getUnbalances node = 
            seq {
                let unbalance = node |> getUnbalance
                match unbalance with
                | Some(child, correctedWeight) ->
                    yield correctedWeight
                    yield! getUnbalances child
                | None -> ()
            }
        root |> getUnbalances |> Seq.last
