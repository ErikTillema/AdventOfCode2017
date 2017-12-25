namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem14

type Problem14Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver   "flqrgnkx" |> should equal 8108

    [<Fact>]
    member x.solveGold_works () = 
         solveGold     "flqrgnkx" |> should equal 1242

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver "xlqgujun" |> should equal 8204

    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold "xlqgujun" |> should equal 1089

     