namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem3

type Problem3Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 1 |> should equal 0
         solveSilver 12 |> should equal 3
         solveSilver 23 |> should equal 2
         solveSilver 1024 |> should equal 31

    [<Fact>]
    member x.solveGold_works () = 
         solveGold 60 |> should equal 122
         solveGold 400 |> should equal 747

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver 277678 |> should equal 475

    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold 277678 |> should equal 279138

     