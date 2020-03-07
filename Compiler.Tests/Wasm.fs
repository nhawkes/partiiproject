module Wasm

open Xunit
open FsCheck
open FsCheck.Xunit
open Generators

open Wasm

[<Property(Arbitrary=[|typeof<Generators>|])>]
let ``Emits``(m:WasmModule) =
    Emit.emitWasmModule m |> ignore
    ()