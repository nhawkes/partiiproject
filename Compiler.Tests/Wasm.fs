module Wasm

open Xunit
open FsCheck
open FsCheck.Xunit
open Generators
open Utils
open Wasm

[<Property(Arbitrary=[|typeof<Generators>|])>]
let ``Emits``(m:WasmModule) =
    Emit.emitWasmModule m |> ignore
    ()


[<Fact>]
let ``WASM Fibinacci 7``() =
    let wasmModule =
        [ TypeSec [ [ I32 ], [ I32 ] ]
          FuncSec [ 0u ]
          ExportSec
              [ { nm = "Fibonacci"
                  exportdesc = ExportFunc 0u } ]
          CodeSec
              [ [],
                [ LocalGet 0u
                  I32Const 0
                  I32Eq
                  IfElse
                      ([ I32 ], [ I32Const 1 ],
                       [ LocalGet 0u
                         I32Const 1
                         I32Eq
                         IfElse
                             ([ I32 ], [ I32Const 1 ],
                              [ LocalGet 0u
                                I32Const 1
                                I32Sub
                                Call 0u

                                LocalGet 0u
                                I32Const 2
                                I32Sub
                                Call 0u

                                I32Add ]) ]) ] ] ]
    let fibonacciProgram = Emit.emitWasmModule wasmModule |> compile
    let output = fibonacciProgram.Exports?Fibonacci (7)
    Assert.Equal(21, output)