namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem6

type Problem6Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver [| 0; 2; 7; 0 |] |> should equal 5

    [<Fact>]
    member x.solveGold_works () = 
         solveGold [| 0; 2; 7; 0 |] |> should equal 4

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver [| 2; 8; 8; 5; 4; 2; 3; 1; 5; 5; 1; 2; 15; 13; 5; 14 |] |> should equal 3156

    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold [| 2; 8; 8; 5; 4; 2; 3; 1; 5; 5; 1; 2; 15; 13; 5; 14 |] |> should equal 1610

     