namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem19
open System.Resources

type Problem19Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver "     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ " |> should equal "ABCDEF"

    [<Fact>]
    member x.solveGold_works () = 
         solveGold     "     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ " |> should equal 38

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveSilver (res.GetString("problem19.in")) |> should equal "GSXDIPWTU"

    [<Fact>]
    member x.solveGold_isAccepted () = 
        let res = ResourceManager("Resources", System.Reflection.Assembly.GetExecutingAssembly())
        solveGold (res.GetString("problem19.in")) |> should equal 16100

     