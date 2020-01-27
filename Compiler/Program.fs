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
         |> CoreGen.genProgram

    let fv = coreModule |> fvProgram
    printfn "%A" coreModule
    match fv |> Set.toList with
    |(Vars.V x,_)::_ ->failwithf "Not defined %s" x.text
    |x::_ -> failwithf "Internal error - not defined: %A" x
    |_ ->

    printfn "%s" (printProgram coreModule)         
    // let coreModule = coreModule |> Transform.transform |> Core.mapProgram (fun v -> v.var)

    
    let stgModule = 
        coreModule
         |> StgGen.genProgram   
    let wasmModule =
        stgModule          
         |> WasmGen.genProgram
    printfn "%A" stgModule
    let bytes = Emit.emitWasmModule wasmModule |> List.toArray
    IO.File.WriteAllBytes("./Compiler.Benchmark/out/wasm/fibonacci.wasm", bytes)
    
    
    0
