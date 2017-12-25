namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem22
open System.Resources

type Problem22Test() = 
    
    [<Fact>]
    member x.solveSilver_works () =
        solveSilver 70 (
            "..#\n"+
            "#..\n"+
            "..."    ) |> should equal 41
        solveSilver 10000 (
            "..#\n"+
            "#..\n"+
            "..."    ) |> should equal 5587

    [<Fact>]
    member x.solveGold_works () = 
        solveGold 100 (
            "..#\n"+
            "#..\n"+
            "..."    ) |> should equal 26
        solveGold 10_000_000 (
            "..#\n"+
            "#..\n"+
            "..."    ) |> should equal 2_511_944

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver 10000 (res.GetString("problem22.in")) |> should equal 5538

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold 10_000_000 (res.GetString("problem22.in")) |> should equal 2_511_090

     