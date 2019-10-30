module Tests

open System
open Xunit
open Emit
open Wasm

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

let arrayFromTuple =
    function
    | t when Reflection.FSharpType.IsTuple(t.GetType()) -> Reflection.FSharpValue.GetTupleFields t
    | x -> [| x |]

let (?) (this: 'Source) (prop: string): obj -> 'Result =
    let p = this.GetType().GetMethod(prop)
    fun x -> p.Invoke(this, arrayFromTuple x) :?> 'Result

let compile byteList =
    let bytes = byteList |> List.toArray
    use stream = new IO.MemoryStream(bytes)
    WebAssembly.Runtime.Compile.FromBinary(stream).Invoke(Collections.Generic.Dictionary<_, _>())


[<Fact>]
let ``Fibinacci 7``() =
    let fibonacciProgram = emitWasmModule wasmModule |> compile
    let output = fibonacciProgram.Exports?Fibonacci (7)
    Assert.Equal(21, output)
