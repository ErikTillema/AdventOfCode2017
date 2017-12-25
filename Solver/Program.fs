// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Resources

[<EntryPoint>]
let main argv = 
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
    
    //res.GetString("problem1.in")                                |> Problem1.solveSilver |> printfn "%d"
    //res.GetString("problem1.in")                                |> Problem1.solveGold |> printfn "%d"
    //res.GetString("problem2.in")                                |> Problem2.solveSilver |> printfn "%d"
    //res.GetString("problem2.in")                                |> Problem2.solveGold |> printfn "%d"
    //277678                                                      |> Problem3.solveSilver |> printfn "%d"
    //277678                                                      |> Problem3.solveGold |> printfn "%d"
    //res.GetString("problem4.in")                                |> Problem4.solveSilver |> printfn "%d"
    //res.GetString("problem4.in")                                |> Problem4.solveGold |> printfn "%d"
    //res.GetString("problem5.in")                                |> Problem5.solveSilver |> printfn "%d"
    //res.GetString("problem5.in")                                |> Problem5.solveGold |> printfn "%d"
    //[| 2; 8; 8; 5; 4; 2; 3; 1; 5; 5; 1; 2; 15; 13; 5; 14 |]     |> Problem6.solveSilver |> printfn "%d"
    //[| 2; 8; 8; 5; 4; 2; 3; 1; 5; 5; 1; 2; 15; 13; 5; 14 |]     |> Problem6.solveGold |> printfn "%d"
    //res.GetString("problem7.in")                                |> Problem7.solveSilver |> printfn "%s"
    //res.GetString("problem7.in")                                |> Problem7.solveGold |> printfn "%d"
    //res.GetString("problem8.in")                                |> Problem8.solveSilver |> printfn "%d"
    //res.GetString("problem8.in")                                |> Problem8.solveGold |> printfn "%d"
    //res.GetString("problem9.in")                                |> Problem9.solveSilver |> printfn "%d"
    //res.GetString("problem9.in")                                |> Problem9.solveGold |> printfn "%d"
    //[ 183;0;31;146;254;240;223;150;2;206;161;1;255;232;199;88 ] |> Problem10.solveSilver 256 |> printfn "%d"
    //"183,0,31,146,254,240,223,150,2,206,161,1,255,232,199,88"   |> Problem10.solveGold |> printfn "%s"
    //res.GetString("problem11.in")                               |> Problem11.solveSilver |> printfn "%d"
    //res.GetString("problem11.in")                               |> Problem11.solveGold |> printfn "%d"
    //res.GetString("problem12.in")                               |> Problem12.solveSilver |> printfn "%d"
    //res.GetString("problem12.in")                               |> Problem12.solveGold |> printfn "%d"
    //res.GetString("problem13.in")                               |> Problem13.solveSilver |> printfn "%d"
    //res.GetString("problem13.in")                               |> Problem13.solveGold |> printfn "%d"
    //"xlqgujun"                                                  |> Problem14.solveSilver |> printfn "%d"
    //"xlqgujun"                                                  |> Problem14.solveGold |> printfn "%d"
    //[| 289L; 629L |]                                            |> Problem15.solveSilver |> printfn "%d"
    //[| 289L; 629L |]                                            |> Problem15.solveGold |> printfn "%d"
    //res.GetString("problem16.in")                               |> Problem16.solveSilver 16 |> printfn "%s"
    //res.GetString("problem16.in")                               |> Problem16.solveGold 16 |> printfn "%s"
    //355                                                        |> Problem17.solveSilver |> printfn "%d"
    //355                                                        |> Problem17.solveGold |> printfn "%d"
    //res.GetString("problem18.in")                               |> Problem18a.solveSilver |> printfn "%d"
    //res.GetString("problem18.in")                               |> Problem18b.solveGold |> printfn "%d"
    //res.GetString("problem19.in")                               |> Problem19.solveSilver |> printfn "%s"
    //res.GetString("problem19.in")                               |> Problem19.solveGold |> printfn "%d"
    //res.GetString("problem19.in")                               |> Problem19.solveGold |> printfn "%s"
    //res.GetString("problem20.in")                               |> Problem20.solveSilver |> printfn "%d"
    //res.GetString("problem20.in")                               |> Problem20.solveGold |> printfn "%d"
    //res.GetString("problem21.in")                              |> Problem21.solveSilver 5 |> printfn "%d"
    //res.GetString("problem21.in")                              |> Problem21.solveSilver 18 |> printfn "%d"
    //res.GetString("problem22.in")                               |> Problem22.solveSilver 10000 |> printfn "%d"
    //res.GetString("problem22.in")                               |> Problem22.solveGold 10_000_000 |> printfn "%d"
    //res.GetString("problem23.in")                               |> Problem23.solveSilver |> printfn "%d"
    //Problem23.solveGold() |> printfn "%d"
    //res.GetString("problem24.in")                               |> Problem24.solveSilver |> printfn "%d"
    //res.GetString("problem24.in")                               |> Problem24.solveGold |> printfn "%d"
    Problem25.solveSilver() |> printfn "%d"

    //res.GetString("problemInfi.in")                             |> ProblemInfi.solveSilver |> printfn "%d"
    //res.GetString("problemInfi.in")                             |> ProblemInfi.solveGold |> ignore

    stopWatch.Stop()
    printfn ""
    printfn "Elapsed: %f ms" stopWatch.Elapsed.TotalMilliseconds
    0 // return an integer exit code
