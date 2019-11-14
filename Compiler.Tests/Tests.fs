module Tests

open System
open Xunit
open Emit
open Wasm
open Stg

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


let stgModule: Program<string> =
    [ "Fibonacci",
      (([ "x" ], [], [ "a"; "b" ]),
       Case
           (Prim [ AVar "x" ], "x",
            PAlts
                ([ I32Const 0, Prim [ ALit(I32Const 1) ] ],
                 Case
                     (Prim [ AVar "x" ], "x",
                      PAlts
                          ([ I32Const 1, Prim [ ALit(I32Const 1) ] ],
                           Case
                               (Prim
                                   [ AVar "Fibonacci"
                                     ALit I32Sub
                                     ALit(I32Const 1)
                                     AVar "x" ], "a",
                                PAlts
                                    ([],
                                     Case
                                         (Prim
                                             [ AVar "Fibonacci"
                                               ALit(I32Sub)
                                               ALit(I32Const 2)
                                               AVar("x") ], "b",
                                          PAlts
                                              ([],
                                               Prim
                                                   [ ALit I32Add
                                                     AVar "a"
                                                     AVar "b" ]))))))))) ]

let arrayFromTuple =
    function
    | null -> [||]
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
let ``WASM Fibinacci 7``() =
    let fibonacciProgram = emitWasmModule wasmModule |> compile
    let output = fibonacciProgram.Exports?Fibonacci (7)
    Assert.Equal(21, output)


[<Fact>]
let Malloc() =
    let wasmModule =
        [ TypeSec [ [], [ I32 ] ]
          FuncSec [ 0u ]
          MemSec [ Min 1u ]
          GlobalSec
              [ { gt = I32, Var
                  init = [ I32Const 0 ] } ]
          ExportSec
              [ { nm = "Malloc"
                  exportdesc = ExportFunc 0u }
                { nm = "Memory"
                  exportdesc = ExportMem 0u } ]
          CodeSec [ [], WasmGen.malloc 13 ] ]

    let mallocProgram = emitWasmModule wasmModule |> compile
    let memory = mallocProgram.Exports?Memory
    let output = mallocProgram.Exports?Malloc ()
    Assert.Equal("13", string(memory))

[<Fact>]
let ``STG Fibinacci 7``() =
    let fibonacciProgram =
        stgModule
        |> WasmGen.genProgram
        |> emitWasmModule
        |> compile

    let output = fibonacciProgram.Exports?Fibonacci (7)
    Assert.Equal(21, output)
