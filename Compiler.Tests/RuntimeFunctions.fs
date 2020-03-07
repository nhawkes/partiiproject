module RuntimeFunctions

open Utils
open Xunit
open Wasm
open Emit

[<Fact>]
let Malloc() =
    let wasmModule =
        [ TypeSec [ RuntimeFunctions.malloc.functype ]
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
          CodeSec [ RuntimeFunctions.malloc.func ] ]

    let mallocProgram = emitWasmModule wasmModule |> compile
    let (memory: WebAssembly.Runtime.UnmanagedMemory) = mallocProgram.Exports?Memory ()
    let output1 = mallocProgram.Exports?Malloc (12)
    let output2 = mallocProgram.Exports?Malloc (12)
    Assert.Equal(20, output2)

[<Fact>]
let Clone() =
    let wasmModule =
        [ TypeSec
            [ (RuntimeFunctions.clone 1u).functype
              RuntimeFunctions.malloc.functype ]
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
          CodeSec
              [ (RuntimeFunctions.clone 1u).func
                RuntimeFunctions.malloc.func ] ]

    let cloneProgram = emitWasmModule wasmModule |> compile
    let (memory: WebAssembly.Runtime.UnmanagedMemory) = cloneProgram.Exports?Memory ()
    printfn ("%A") memory
    let output1 = cloneProgram.Exports?Malloc (12)
    System.Runtime.InteropServices.Marshal.Copy([| 12; 1; 2; 3 |], 0, memory.Start, 4)
    let output2 = cloneProgram.Exports?Clone (4)
    let output1 = cloneProgram.Exports?Malloc (12)
    Assert.Equal(20, output2)

[<Fact>]
let Apply() =
    let wasmModule =
        [ TypeSec
            [ RuntimeFunctions.stdFuncType
              (RuntimeFunctions.apply 0u).functype ]
          FuncSec [ 1u ]
          TableSec [ Wasm.Table(Wasm.FuncRef, Wasm.MinMax(1u, 1u)) ]
          MemSec [ Min 1u ]
          GlobalSec
              [ { gt = I32, Var
                  init = [ I32Const 0 ] } ]
          ExportSec
              [ { nm = "Apply"
                  exportdesc = ExportFunc 0u }
                { nm = "Memory"
                  exportdesc = ExportMem 0u } ]
          CodeSec [ (RuntimeFunctions.apply 0u).func ] ]

    let applyProgram = emitWasmModule wasmModule |> compile
    let (memory: WebAssembly.Runtime.UnmanagedMemory) = applyProgram.Exports?Memory ()
    printfn ("%A") memory
    System.Runtime.InteropServices.Marshal.Copy([| 12; 20; 1; 0 |], 0, memory.Start, 4)
    printfn "%A" (memoryToArray memory)
    let output2 = applyProgram.Exports?Apply (4, 14)
    printfn "%A" (memoryToArray memory)
    let output2 = applyProgram.Exports?Apply (4, 13)
    printfn "%A" (memoryToArray memory)
    let output2 = applyProgram.Exports?Apply (4, 12)
    printfn "%A" (memoryToArray memory)
    let output2 = applyProgram.Exports?Apply (4, 11)
    printfn "%A" (memoryToArray memory)
    Assert.Equal(4, output2)