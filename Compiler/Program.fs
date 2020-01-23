// Learn more about F# at http://fsharp.org

open System
open Core
open StgGen
open Ast

let program = """
data Cons(head, tail)
data Empty()
map(f(x), list) = {     return switch(list){         | Cons(head, tail) => Cons(f(head), tail)         | Empty() => Empty     } } take(i, list) = {     return switch(list){         | Cons(head, tail) =>              switch(i){                 | 0 => head                 | _ => take(i-1, tail)             }     } } export fibonacci(x) = {     list = Cons(1, Cons(5, Cons(9, Empty)))     addTwo(x) = x + 2     mapped = map(addTwo, list)     mapped2 = map(addTwo, list)     mapped3 = map(addTwo, list)     mapped4 = map(addTwo, list)     mapped5 = map(addTwo, list)     return take(0, mapped) }  """  


[<EntryPoint>] 
let main argv =
    match Parser.parse program with
    |Error err -> failwith err
    |Ok astModule ->
    let coreModule =
        astModule 
         |> Renamer.renameProgram
         |> CoreGen.genProgram
    //let coreModule = coreModule |> Transform.transform |> Core.mapProgram (fun v -> v.var)
    let stgModule = 
        coreModule
         |> StgGen.genProgram   
    let wasmModule =
        stgModule          
         |> WasmGen.genProgram
    let bytes = Emit.emitWasmModule wasmModule |> List.toArray
    IO.File.WriteAllBytes("./Compiler.Benchmark/out/wasm/fibonacci.wasm", bytes)
    
    0
