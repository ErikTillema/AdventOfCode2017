namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem1
open System.Resources

type Problem1Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "1122" |> should equal 3
         solveSilver "1111" |> should equal 4
         solveSilver "1234" |> should equal 0
         solveSilver "91212129" |> should equal 9

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "1212" |> should equal 6
         solveGold "1221" |> should equal 0
         solveGold "123425" |> should equal 4
         solveGold "123123" |> should equal 12
         solveGold "12131415" |> should equal 4

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem1.in")) |> should equal 1393

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem1.in")) |> should equal 1292

     