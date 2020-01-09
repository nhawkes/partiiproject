// Learn more about F# at http://fsharp.org

open System
open Core
open StgGen
open Ast

let program = """
data List(head, tail)
fibonacciList(a, b) = {
    return List(a, fibonacciList(b, a+b))
}
take(n, list) = {
    return switch(list){
        | List(head, tail) => switch(n){
            | 0 => head
            | _ => take(n-1, tail)
        }
    }
}
answer = take(7, fibonacciList(1,1))
export fibonacci(x) = {
    return answer
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
