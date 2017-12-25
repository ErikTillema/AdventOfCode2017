namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem5
open System.Resources

type Problem5Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver   "0
                        3
                        0
                        1
                        -3" |> should equal 5

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "0
                    3
                    0
                    1
                    -3" |> should equal 10

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem5.in")) |> should equal 343364

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem5.in")) |> should equal 25071947

     