// Learn more about F# at http://fsharp.org

open System
open System.IO
open Core
open StgGen
open Ast



let compile file =
    match Parser.parse file with
    |Error err -> failwith err
    |Ok astModule ->
    let coreModule =
        astModule 
         |> CoreGen.genProgram

    let fv = coreModule |> fvProgram
    match fv |> Set.toList with
    |(x,_)::_ ->failwithf "Not defined %s" x
    |_ ->
   
    let coreModule = coreModule |> Transform.transform
    
    let stgModule = 
        coreModule
         |> StgGen.genProgram   
    let moduleName = Path.GetFileNameWithoutExtension file
    let wasmModule =
        stgModule          
         |> WasmGen.genProgram moduleName
    let bytes = Emit.emitWasmModule wasmModule |> List.toArray
    bytes
    
    
    

[<EntryPoint>] 
let main argv =
    let directory = "Examples"    
    let outdir = DirectoryInfo("./Compiler.Benchmark/out/wasm/")
    
    for file in DirectoryInfo(directory).GetFiles() do
        try
            let compiled = compile file.FullName
            let outfile = Path.Join(outdir.FullName, Path.GetFileNameWithoutExtension(file.Name)+".wasm")        
            IO.File.WriteAllBytes(outfile, compiled)
            printfn "Successfully compiled %s" file.Name
        with
        |e -> 
            raise e
            printfn "Failed to compile %s with error\n%A" file.Name e
    
    0
