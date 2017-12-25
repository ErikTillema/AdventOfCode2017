namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem16
open System.Resources

type Problem16Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 5 "s1,x3/4,pe/b" |> should equal "baedc"

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver 16 (res.GetString("problem16.in")) |> should equal "doeaimlbnpjchfkg"

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold 16 (res.GetString("problem16.in")) |> should equal "agndefjhibklmocp"

     