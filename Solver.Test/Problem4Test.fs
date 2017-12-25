namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem4
open System.Resources

type Problem4Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver   "aa bb cc dd ee
                        aa bb cc dd aa
                        aa bb cc dd aaa" |> should equal 2

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "abcde fghij
                    abcde xyz ecdab
                    a ab abc abd abf abj
                    iiii oiii ooii oooi oooo
                    oiii ioii iioi iiio" |> should equal 3

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem4.in")) |> should equal 455

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem4.in")) |> should equal 186

     