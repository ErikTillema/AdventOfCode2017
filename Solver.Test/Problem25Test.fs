namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem25

type Problem25Test() = 
    
    [<Fact>]
    member x.solveSilver_isAccepted () = 
         solveSilver() |> should equal 2870
