namespace Solver.Test

open System
open Xunit
open FsUnit.Xunit
open Problem10

type Problem10Test() = 
    
    [<Fact>]
    member x.solveSilver_works () = 
         solveSilver 5 [ 3; 4; 1; 5 ] |> should equal 12

    [<Fact>]
    member x.solveGold_works () = 
         solveGold "" |> should equal "a2582a3a0e66e6e86e3812dcb672a272"
         solveGold "AoC 2017" |> should equal "33efeb34ea91902bb2f59c9920caa6cd"
         solveGold "1,2,3" |> should equal "3efbe78a8d82f29979031a4aa0b16a9d"
         solveGold "1,2,4" |> should equal "63960835bcdc130f0b66d7ff4f6a5a8e"

    [<Fact>]
    member x.solveSilver_isAccepted () = 
        solveSilver 256 [ 183;0;31;146;254;240;223;150;2;206;161;1;255;232;199;88 ] |> should equal 15990

    [<Fact>]
    member x.solveGold_isAccepted () = 
        solveGold "183,0,31,146,254,240,223,150,2,206,161,1,255,232,199,88" |> should equal "90adb097dd55dea8305c900372258ac6"

     