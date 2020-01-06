// Learn more about F# at http://fsharp.org

open System
open Core
open StgGen
open Ast

let program = """
data List(head, tail)
numbersFrom(i) = {
    return List(i, numbersFrom(i+1))
}
export fibonacci(x) = {
    return switch(numbersFrom(0)){
        |List(_, i) => i
    }
}
"""
"""
data Box(v)
box(i) = {
    a = i
    return Box(a)
}
export fibonacci(x) = {
    return switch(box(x)){
        |Box(i)=>i
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
