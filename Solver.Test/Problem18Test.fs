namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem18a
open Problem18b
open System.Resources

type Problem18Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver   "set a 1
                        add a 2
                        mul a a
                        mod a 5
                        snd a
                        set a 0
                        rcv a
                        jgz a -1
                        set a 1
                        jgz a -2" |> should equal 4L

    [<Fact>]
    member x.solveGold_works () = 
         solveGold     "snd 1
                        snd 2
                        snd p
                        rcv a
                        rcv b
                        rcv c
                        rcv d" |> should equal 3

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem18.in")) |> should equal 9423L

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem18.in")) |> should equal 7620

     