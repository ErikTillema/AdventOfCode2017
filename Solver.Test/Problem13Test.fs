namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem13
open System.Resources

type Problem13Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver   "0: 3
                        1: 2
                        4: 4
                        6: 4" |> should equal 24

    [<Fact>]
    member x.solveGold_works () = 
         solveGold     "0: 3
                        1: 2
                        4: 4
                        6: 4" |> should equal 10

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem13.in")) |> should equal 1876

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem13.in")) |> should equal 3964778

     