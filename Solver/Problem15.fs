module Problem15

    let generateNext i value =
        let factor = [| 16807L; 48271L |]
        let m = 2147483647L
        (value * factor.[i]) % m

    let rec generateNextImproved i value =
        let m = [|4L;8L|]
        match generateNext i value with
        | a when a % m.[i] = 0L-> a
        | a -> generateNextImproved i a
                
    let getSequence start i =
        // unfold is a factor 2 faster it seems.
        Seq.unfold (fun s -> Some(s, generateNext i s)) (generateNext i start)
        //seq {
        //    let a = (generateNext i start)
        //    yield a
        //    yield! getSequence a i
        //}

    let getSequenceImproved start i =
        Seq.unfold (fun s -> Some(s, generateNextImproved i s)) (generateNextImproved i start)
    
    let isOk (val1, val2) =
        let m = 65536L
        (val1%m) = (val2%m)
 
    let solveSilver [|startA; startB|] =
        let seqA = getSequence startA 0
        let seqB = getSequence startB 1
        Seq.zip seqA seqB |> Seq.take 40_000_000 |> Seq.filter isOk |> Seq.length
        // The imperative way is three times faster. The use of sequences has its cost...
        //let mutable result = 0
        //let value = [|startA; startB|]
        //let m = 2147483647L
        //let m2 = 65536L
        //for _ in 1..40_000_000 do
        //    for i in 0..1 do
        //        value.[i] <- (value.[i] * factor.[i]) % m
        //    if (value.[0] % m2) = (value.[1] % m2) then result <- result + 1
        //result
       
    let solveGold [|startA; startB|]  =
        let seqA = getSequenceImproved startA 0
        let seqB = getSequenceImproved startB 1
        Seq.zip seqA seqB |> Seq.take 5_000_000 |> Seq.filter isOk |> Seq.length
