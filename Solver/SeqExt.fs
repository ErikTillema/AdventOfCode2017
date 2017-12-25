module SeqExt

    open System
    open System.Collections.Generic

    let getGenerator (sequence: 'a seq) = 
        let enumerator = sequence.GetEnumerator()
        fun () -> 
            match enumerator.MoveNext() with
            | true -> Some(enumerator.Current)
            | false -> None
    
    /// Like Seq.unfold, but doesn't require state
    /// This function uses the generator to unfold a sequence, continuing while generator returns Some(element)
    /// and stopping when it returns None.
    /// Works with SeqExt.getGenerator: mySequence |> SeqExt.getGenarator |> SeqExt.unfold2 equals mySequence
    let unfold2 generator =
        let g dummyState = 
            match generator() with
            | Some(element) -> Some(element, dummyState)
            | None -> None
        Seq.unfold g 0
        
    /// returns the first n items of source or less if there are fewer items.
    let takeAtMost n (source: 'a seq) = 
        Seq.truncate n source

    /// returns whether all items in source are equal
    let allEqual source =
        match Seq.tryHead source with
        | None -> true
        | Some(h) -> source |> Seq.forall ((=) h)
    
    /// returns the Cartesian product of 2 sequences
    let cartesianProduct seq1 seq2 = 
        let array2 = seq2 |> Seq.toArray
        seq1 |> Seq.collect (fun el1 ->
            array2 |> Seq.map (fun el2 -> (el1, el2))
        )

    let rng = new Random()

    /// returns a shuffled version of original (as array)
    let shuffle (original : 'a seq) =
        let res = Seq.toArray original
        let n = res.Length
        for x in 1..n do
            let i = n-x
            let j = rng.Next(i+1)
            let tmp = res.[i]
            res.[i] <- res.[j]
            res.[j] <- tmp
        res

    /// returns all subsets of source (as lists)
    let getSubsets (source: 'a seq) = 
        let source = source |> Seq.toArray
        let rec getSubsets' don acc =
            seq {
                if don = source.Length then
                    yield List.rev acc
                else
                    yield! getSubsets' (don+1) acc
                    yield! getSubsets' (don+1) (source.[don] :: acc)
            }
        getSubsets' 0 []

    /// returns all combinations of source (as arrays)
    let getChooseCombinations n (source: 'a seq) = 
        let source = source |> Seq.toArray
        let sourceCount = source.Length
        let result = Array.zeroCreate n
        let rec getChooseCombinations' don ins =
            seq {
                if ins = n then
                    yield result
                else
                    let remainingItemsAfterThisOne = sourceCount - don - 1
                    // try to put in result
                    if ins < n then
                        result.[ins] <- source.[don]
                        yield! getChooseCombinations' (don+1) (ins+1)

                    // try to leave out of result
                    if ins + remainingItemsAfterThisOne >= n then
                        yield! getChooseCombinations' (don+1) ins
            }
        getChooseCombinations' 0 0

    /// returns all permutations of source (as arrays)
    let getPermutations (source: 'a seq) = 
        let source = source |> Seq.toArray
        let n = source.Length
        let result = Array.zeroCreate n
        let used = Array.create n false
        let rec getPermutations' don =
            seq {
                if don = n then
                    yield result
                else
                    for i in 0..n-1 do
                        if not used.[i] then
                            used.[i] <- true
                            result.[don] <- source.[i]
                            yield! getPermutations' (don+1)
                            used.[i] <- false
            }
        getPermutations' 0

    /// returns all rotations of source (as lists)
    let getRotations (source: 'a seq) = 
        let source = source |> Seq.toList
        let n = source.Length
        seq {
            if n = 0 then
                yield []
            else
                for i in 0..n-1 do
                    yield (source |> List.skip i |> List.take (n-i)) @ (source |> List.take i)
        }
        