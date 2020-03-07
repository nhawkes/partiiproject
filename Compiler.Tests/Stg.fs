module Stg
open Xunit
open FsCheck
open FsCheck.Xunit
open Generators

[<Property(Arbitrary=[|typeof<Generators>|])>]
let ``Gen Stg from Core``(core:Core.ClosedProgram<_>)=
    let result = StgGen.genProgram core
    Assert.True(true)