namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem12
open System.Resources

type Problem12Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver   "0 <-> 2
                        1 <-> 1
                        2 <-> 0, 3, 4
                        3 <-> 2, 4
                        4 <-> 2, 3, 6
                        5 <-> 6
                        6 <-> 4, 5" |> should equal 6
         solveSilver   "1 <-> 1
                        5 <-> 6
                        6 <-> 4, 5
                        2 <-> 0, 3, 4
                        3 <-> 2, 4
                        4 <-> 2, 3, 6
                        0 <-> 2" |> should equal 6

    [<Fact>]
    member x.solveGold_works () = 
         solveGold     "0 <-> 2
                        1 <-> 1
                        2 <-> 0, 3, 4
                        3 <-> 2, 4
                        4 <-> 2, 3, 6
                        5 <-> 6
                        6 <-> 4, 5" |> should equal 2

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem12.in")) |> should equal 169

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem12.in")) |> should equal 179

     