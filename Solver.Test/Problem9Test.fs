namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem9
open System.Resources

type Problem9Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "{}" |> should equal 1
         solveSilver "{{{}}}" |> should equal 6
         solveSilver "{{},{}}" |> should equal 5
         solveSilver "{{{},{},{{}}}}" |> should equal 16
         solveSilver "{<a>,<a>,<a>,<a>}" |> should equal 1
         solveSilver "{{<ab>},{<ab>},{<ab>},{<ab>}}" |> should equal 9
         solveSilver "{{<!!>},{<!!>},{<!!>},{<!!>}}" |> should equal 9
         solveSilver "{{<a!>},{<a!>},{<a!>},{<ab>}}" |> should equal 3

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "<>" |> should equal 0
         solveGold "<random characters>" |> should equal 17
         solveGold "<<<<>" |> should equal 3
         solveGold "<{!>}>" |> should equal 2
         solveGold "<!!>" |> should equal 0
         solveGold "<!!!>>" |> should equal 0
         solveGold "<{o\"i!a,<{i<a>" |> should equal 10

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem9.in")) |> should equal 14212

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem9.in")) |> should equal 6569

     