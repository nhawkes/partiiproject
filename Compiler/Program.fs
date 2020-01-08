﻿// Learn more about F# at http://fsharp.org

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
    printfn "Core: %A" coreModule
    let stgModule = 
        coreModule
         |> StgGen.genProgram   
    printfn "Stg: %A" stgModule
    let wasmModule =
        stgModule          
         |> WasmGen.genProgram
    let bytes = Emit.emitWasmModule wasmModule |> List.toArray
    IO.File.WriteAllBytes("./Compiler.Benchmark/out/wasm/fibonacci.wasm", bytes)
    
    0
