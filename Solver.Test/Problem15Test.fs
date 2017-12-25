namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem15

type Problem15Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver [| 65L; 8921L |] |> should equal 588

    [<Fact>]
    member x.solveGold_works () = 
         solveGold [| 65L; 8921L |] |> should equal 309

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver [| 289L; 629L |] |> should equal 638 

    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold [| 289L; 629L |] |> should equal 343

     