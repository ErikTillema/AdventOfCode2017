namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem24
open System.Resources

type Problem24Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver   "0/2
                        2/2
                        2/3
                        3/4
                        3/5
                        0/1
                        10/1
                        9/10" |> should equal 31

    [<Fact>]
    member x.solveGold_works () = 
         solveGold     "0/2
                        2/2
                        2/3
                        3/4
                        3/5
                        0/1
                        10/1
                        9/10" |> should equal 19

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem24.in")) |> should equal 2006

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem24.in")) |> should equal 1994

     