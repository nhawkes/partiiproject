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

let (?) (self: 'Source) (prop: string): obj -> 'Result =
    let p = self.GetType().GetMethod(prop)
    if (isNull p) then
        fun _ -> self.GetType().GetProperty(prop).GetValue(self) :?> 'Result
    else
        fun x -> p.Invoke(self, arrayFromTuple x) :?> 'Result

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


let memoryToArray (memory:WebAssembly.Runtime.UnmanagedMemory) =
    let size = 10
    let (array:int array) = Array.create size 0
    System.Runtime.InteropServices.Marshal.Copy(memory.Start, array, 0, size)
    array

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
    let (memory:WebAssembly.Runtime.UnmanagedMemory) = mallocProgram.Exports?Memory()
    let output1 = mallocProgram.Exports?Malloc (12)
    let output2 = mallocProgram.Exports?Malloc (12)
    Assert.Equal(20, output2)

[<Fact>]
let Clone() =
    let wasmModule =
        [ TypeSec [ (WasmGen.clone 1u).functype; WasmGen.malloc.functype ]
          FuncSec [ 0u; 1u ]
          MemSec [ Min 1u ]
          GlobalSec
              [ { gt = I32, Var
                  init = [ I32Const 0 ] } ]
          ExportSec
              [ { nm = "Clone"
                  exportdesc = ExportFunc 0u }
                { nm = "Malloc"
                  exportdesc = ExportFunc 1u }
                { nm = "Memory"
                  exportdesc = ExportMem 0u } ]
          CodeSec [ (WasmGen.clone 1u).func; WasmGen.malloc.func  ] ]

    let cloneProgram = emitWasmModule wasmModule |> compile
    let (memory:WebAssembly.Runtime.UnmanagedMemory) = cloneProgram.Exports?Memory()
    printfn("%A") memory
    let output1 = cloneProgram.Exports?Malloc (12)
    System.Runtime.InteropServices.Marshal.Copy([|12;1;2;3|], 0, memory.Start, 4)
    let output2 = cloneProgram.Exports?Clone (4)
    let output1 = cloneProgram.Exports?Malloc (12)
    Assert.Equal(20, output2)
