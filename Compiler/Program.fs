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

let astModule : Program<Var> =
    [
        TypeDecl(builtInVar IntegerConstr, [localVar "i" IntT])
        GlobalDecl((globalVar "add" (TopFuncT([ValueT; ValueT], ValueT))),
            [localVar "x_boxed" ValueT; localVar "y_boxed" ValueT], 
            Block [Return(
                Match(
                    (Var((localVar "x_boxed" ValueT)), ValueT),
                    [
                        PatConstr(builtInVar IntegerConstr, [PatBind (localVar "x" IntT)]),
                        Match(
                            (Var((localVar "y_boxed" ValueT)), ValueT),
                            [
                                PatConstr(builtInVar IntegerConstr, [PatBind (localVar "y" IntT)]),
                                Match(
                                    (Prim [PrimWasm Wasm.I32Add; PrimVar (localVar "x" IntT); PrimVar (localVar "y" IntT)], IntT),
                                    [
                                        PatBind(localVar "r" IntT), (Call (builtInVar IntegerConstr, [Var (localVar "r" IntT)]))
                                    ]
                                )
                            ]
                        )
                    ]
                )
            )]
        )        
        GlobalDecl((globalVar "subtract" (TopFuncT([ValueT; ValueT], ValueT))), 
            [localVar "x_boxed" ValueT; localVar "y_boxed" ValueT],                
            Block([Return(
                    Match(
                        (Var((localVar "x_boxed" ValueT)), ValueT),
                        [
                            PatConstr(builtInVar IntegerConstr, [PatBind (localVar "x" IntT)]),
                            Match(
                                (Var((localVar "y_boxed" ValueT)), ValueT),
                                [
                                    PatConstr(builtInVar IntegerConstr, [PatBind (localVar "y" IntT)]),
                                    Match(
                                        (Prim [PrimWasm Wasm.I32Sub; PrimVar (localVar "y" IntT); PrimVar (localVar "x" IntT)], IntT),
                                        [
                                            PatBind(localVar "r" IntT),
                                            Call (builtInVar IntegerConstr, [Var (localVar "r" IntT)])
                                        ]
                                    )
                                ]
                            )
                        ]
                    )
                )]
            )
        )
        
    ]


[<EntryPoint>]
let main argv =
    match Parser.parse program with
    |Error err -> failwith err
    |Ok astModule ->
    printfn "%A" astModule
    (*    
    let coreModule =
        astModule 
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
    *)
    0
