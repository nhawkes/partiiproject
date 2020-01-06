module Tests

open System
open Xunit
open Emit
open Wasm
open Stg
open WasmGen

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
let ``Fibinacci 7``() =
    let program = """
        export Fibonacci(x) = {
            return switch(x){
                | 0 => 1
                | 1 => 1
                | _ => Fibonacci(x-1) + Fibonacci(x-2)
            }
        }
        """
    match Parser.parse program with
    | Error err -> failwith err
    | Ok astModule ->
        let wasmModule =
            astModule
            |> Renamer.renameProgram
            |> CoreGen.genProgram
            |> StgGen.genProgram
            |> WasmGen.genProgram

        let bytes = Emit.emitWasmModule wasmModule |> List.toArray

        let fibonacciProgram = emitWasmModule wasmModule |> compile
        let output = fibonacciProgram.Exports?Fibonacci (7)
        Assert.Equal(21, output)


[<Fact>]
let Malloc() =
    let wasmModule =
        [ TypeSec [ WasmGen.malloc.functype ]
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
          CodeSec [ WasmGen.malloc.func ] ]

    let mallocProgram = emitWasmModule wasmModule |> compile
    let memory = mallocProgram.Exports?Memory
    let output1 = mallocProgram.Exports?Malloc (12)
    let output2 = mallocProgram.Exports?Malloc (12)
    Assert.Equal(20, output2)
