namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem2
open System.Resources

type Problem2Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver   "5 1 9 5
                        7 5 3
                        2 4 6 8" |> should equal 18

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "5 9 2 8
                    9 4 7 3
                    3 8 6 5" |> should equal 9

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem2.in")) |> should equal 42378

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem2.in")) |> should equal 246

     