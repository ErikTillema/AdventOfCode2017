namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem11
open System.Resources

type Problem11Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "ne,ne,ne" |> should equal 3
         solveSilver "ne,ne,sw,sw" |> should equal 0
         solveSilver "ne,ne,s,s" |> should equal 2
         solveSilver "se,sw,se,sw,sw" |> should equal 3

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "ne,ne,ne" |> should equal 3
         solveGold "ne,ne,sw,sw" |> should equal 2
         solveGold "ne,ne,s,s" |> should equal 2
         solveGold "se,sw,se,sw,sw" |> should equal 3

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem11.in")) |> should equal 877

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem11.in")) |> should equal 1622

     