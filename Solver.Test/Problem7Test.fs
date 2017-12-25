namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem7
open System.Resources

type Problem7Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver   "pbga (66)
                        xhth (57)
                        ebii (61)
                        havc (66)
                        ktlj (57)
                        fwft (72) -> ktlj, cntj, xhth
                        qoyq (66)
                        padx (45) -> pbga, havc, qoyq
                        tknk (41) -> ugml, padx, fwft
                        jptl (61)
                        ugml (68) -> gyxo, ebii, jptl
                        gyxo (61)
                        cntj (57)" |> should equal "tknk"

    [<Fact>]
    member x.solveGold_works () = 
         solveGold     "pbga (66)
                        xhth (57)
                        ebii (61)
                        havc (66)
                        ktlj (57)
                        fwft (72) -> ktlj, cntj, xhth
                        qoyq (66)
                        padx (45) -> pbga, havc, qoyq
                        tknk (41) -> ugml, padx, fwft
                        jptl (61)
                        ugml (68) -> gyxo, ebii, jptl
                        gyxo (61)
                        cntj (57)" |> should equal 60

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem7.in")) |> should equal "ykpsek"

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem7.in")) |> should equal 1060

     