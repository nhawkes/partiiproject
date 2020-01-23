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
    let coreModule =
        astModule 
         |> Renamer.renameProgram
         |> CoreGen.genProgram
    // let coreModule = coreModule |> Transform.transform |> Core.mapProgram (fun v -> v.var)
    let stgModule = 
        coreModule
         |> StgGen.genProgram   
    let wasmModule =
        stgModule          
         |> WasmGen.genProgram
    let bytes = Emit.emitWasmModule wasmModule |> List.toArray
    IO.File.WriteAllBytes("./Compiler.Benchmark/out/wasm/fibonacci.wasm", bytes)
    
    0
