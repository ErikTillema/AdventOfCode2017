namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem8
open System.Resources

type Problem8Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver   "b inc 5 if a > 1
						a inc 1 if b < 5
						c dec -10 if a >= 1
						c inc -20 if c == 10" |> should equal 1

    [<Fact>]
    member x.solveGold_works () = 
         solveGold     "b inc 5 if a > 1
						a inc 1 if b < 5
						c dec -10 if a >= 1
						c inc -20 if c == 10" |> should equal 10

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem8.in")) |> should equal 7787

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem8.in")) |> should equal 8997

     