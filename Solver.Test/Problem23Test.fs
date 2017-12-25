namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem23
open System.Resources

type Problem23Test() = 

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem23.in")) |> should equal 4225

    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold() |> should equal 905

     