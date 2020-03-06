// Learn more about F# at http://fsharp.org

open System
open System.IO
open Core
open StgGen
open Ast
open Argu
open GlobExpressions


let compile optimise dumpCore dumpStg file =
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
   
    let coreModule = coreModule |> Transform.transform optimise
    if dumpCore then
        Core.printProgram coreModule |> printfn "%s" 
    let stgModule = 
        coreModule
         |> StgGen.genProgram   
    if dumpStg then
        stgModule |> printfn "%A" 
    let moduleName = Path.GetFileNameWithoutExtension file
    let wasmModule =
        stgModule          
         |> WasmGen.genProgram moduleName
    let bytes = Emit.emitWasmModule wasmModule |> List.toArray
    bytes
    
    


type CliArguments =
    | [<Unique; AltCommandLine("-o");>] OutDir of path:string
    | [<MainCommand; Unique; Last>] In of glob:string
    | [<AltCommandLine("-O1"); Unique>] Optimise
    | [<AltCommandLine("-O0"); Unique>] NoOptimise
    | [<Unique; EqualsAssignment>] Suffix of string
    | [<Unique>] DumpCore
    | [<Unique>] DumpStg
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | OutDir _ -> "specify a output directory."
            | In _ -> "specify a input glob."
            | Optimise _ -> "perform optimisations"
            | NoOptimise _ -> "do not perform optimisations"
            | Suffix _ -> "suffix to add to output files"
            | DumpCore _ -> "print core"
            | DumpStg _ -> "print stg"

[<EntryPoint>] 
let main argv =
    let args = ArgumentParser.Create().Parse(argv)
    let glob = args.GetResult(In, "Examples/**")
    let outdirstr = args.GetResult(OutDir, "./Compiler.Benchmark/out/wasm/")
    let optimise = (args.Contains(Optimise))
    let suffix = args.GetResult(Suffix, "")
    let outdir = DirectoryInfo(outdirstr)
    let dumpCore = args.Contains DumpCore
    let dumpStg = args.Contains DumpStg
    
    let cwd = Directory.GetCurrentDirectory()
    let files = Glob.Files(cwd, glob) |> List.ofSeq
    if files.Length = 0 then 
        printfn "No file matched %s in %s" glob cwd
        -1
    else
    for filestr in files do
        let file = System.IO.FileInfo(filestr)
        try
            let compiled = compile optimise dumpCore dumpStg file.FullName
            let outfile = Path.Join(outdir.FullName, Path.GetFileNameWithoutExtension(file.Name)+suffix+".wasm")        
            IO.File.WriteAllBytes(outfile, compiled)
            printfn "Successfully compiled %s" file.Name
        with
        |e -> 
            raise e
            printfn "Failed to compile %s with error\n%A" file.Name e
    
    0
