namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open ProblemInfi
open System.Resources

type ProblemInfiTest() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "[0,0][1,1](1,0)(0,-1)(0,1)(-1,0)(-1,0)(0,1)(0,-1)(1,0)" |> should equal 2
         solveSilver "[0,0][1,1][9,9](1,0)(0,-1)(0,0)(0,1)(-1,0)(0,0)(-1,0)(0,1)(0,0)(0,-1)(1,0)(0,0)" |> should equal 2

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problemInfi.in")) |> should equal 524
