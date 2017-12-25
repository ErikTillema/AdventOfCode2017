namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem20
open System.Resources

type Problem20Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver   "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
                        p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>" |> should equal 0

    [<Fact>]
    member x.solveGold_works () = 
         solveGold     "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
                        p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
                        p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
                        p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>" |> should equal 1

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem20.in")) |> should equal 161

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem20.in")) |> should equal 438

     