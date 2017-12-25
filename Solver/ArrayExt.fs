module ArrayExt

    open System

    /// returns the first n items of source or less if there are fewer items.
    let takeAtMost n source = 
        Array.truncate n source
        //if Array.length source <= n then
        //    source
        //else
        //    Array.take n source

    /// returns the Cartesian product of 2 sequences
    let cartesianProduct source1 source2 = 
        source1 |> Array.collect (fun el1 ->
            source2 |> Array.map (fun el2 -> (el1, el2))
        ) 
        