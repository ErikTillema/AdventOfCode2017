namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem21
open System.Resources

type Problem21Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 2 ("../.# => ##./#../...\n" + 
                        ".#./..#/### => #..#/..../..../#..#") |> should equal 12

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver 5 (res.GetString("problem21.in")) |> should equal 176

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver 18 (res.GetString("problem21.in")) |> should equal 2368161

     