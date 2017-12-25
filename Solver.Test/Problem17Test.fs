namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem17

type Problem17Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 3 |> should equal 638 

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver 355 |> should equal 1912

    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold 355 |> should equal 21066990

     