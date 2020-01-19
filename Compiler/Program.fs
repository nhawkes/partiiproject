// Learn more about F# at http://fsharp.org

open System
open Core
open StgGen
open Ast

let program = """
export fibonacci(x) = {
    return switch(x){
        | 0 => 1
        | 1 => 1
        | _ => fibonacci(x-1) + fibonacci(x-2)
    }
}
"""

[<EntryPoint>] 
let main argv =
    match Parser.parse program with
    |Error err -> failwith err
    |Ok astModule ->
    printfn "Ast: %A" astModule    
    let coreModule =
        astModule 
         |> Renamer.renameProgram
         |> CoreGen.genProgram
    let optimized = coreModule |> Transform.transform |> Core.mapProgram (fun v -> v.var)
    printfn "Core: %A" coreModule
    printfn "Optimized: %A" optimized
    let stgModule = 
        optimized
         |> StgGen.genProgram   
    printfn "Stg: %A" stgModule
    let wasmModule =
        stgModule          
         |> WasmGen.genProgram
    let bytes = Emit.emitWasmModule wasmModule |> List.toArray
    IO.File.WriteAllBytes("./Compiler.Benchmark/out/wasm/fibonacci.wasm", bytes)
    
    0
