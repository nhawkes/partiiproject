// Learn more about F# at http://fsharp.org

open System
open Core
open StgGen
open Ast

let astModule : Program<Var> =
    [
        TypeDecl(builtInVar IntegerConstr, [localVar "i" IntT])
        GlobalDecl((globalVar "add" (TopFuncT([ValueT; ValueT], ValueT))), 
            Block([localVar "x_boxed" ValueT; localVar "y_boxed" ValueT],
                [Return(
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
        )
        GlobalDecl((globalVar "subtract" (TopFuncT([ValueT; ValueT], ValueT))), 
            Block([localVar "x_boxed" ValueT; localVar "y_boxed" ValueT],
                [Return(
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
        GlobalDecl((globalVar "fibonacci" (TopFuncT([IntT], ValueT))), 
            Block([localVar "x" IntT],
                [
                Assign(localVar "x_boxed" ValueT, Call(builtInVar IntegerConstr, [Var (localVar "x" IntT)]))
                Return(
                    Match(
                        (Call(globalVar "fibonacci_boxed" (TopFuncT([ValueT], ValueT)), [Var(localVar "x_boxed" ValueT)]), ValueT),
                        [
                            PatConstr(builtInVar IntegerConstr, [PatBind (localVar "result" IntT)]),
                            Var((localVar "result" IntT))
                        ]
                    )
                )
                ]
            )
        )
        GlobalDecl((globalVar "fibonacci_boxed" (TopFuncT([ValueT], ValueT))), 
            Block([localVar "x_boxed" ValueT],
                [
                    Return(
                        Match(
                            (Var(localVar "x_boxed" ValueT), ValueT),
                            [
                                PatLit(Box (Integer 0)), Lit(Box (Integer 1))
                                PatLit(Box (Integer 1)), Lit(Box (Integer 1))
                                PatBind(localVar "x_boxed_eval" ValueT), 
                                Call((globalVar "add" (TopFuncT([ValueT; ValueT], ValueT))),
                                    [
                                    Call(globalVar "fibonacci_boxed" (TopFuncT([ValueT], ValueT)),
                                        [
                                        Call(globalVar "subtract" (TopFuncT([ValueT; ValueT], ValueT)),
                                            [
                                                Var(localVar "x_boxed_eval" ValueT)
                                                Lit(Box (Integer 1))
                                            ])
                                        ])
                                    Call(globalVar "fibonacci_boxed" (TopFuncT([ValueT], ValueT)),
                                        [
                                        Call(globalVar "subtract" (TopFuncT([ValueT; ValueT], ValueT)),
                                            [
                                                Var(localVar "x_boxed_eval" ValueT)
                                                Lit(Box (Integer 2))
                                            ])
                                        ])
                                    ]
                                )
                            ]
                        )
                    )
                ]
            )
        )
    ]


[<EntryPoint>]
let main argv =
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
    0
