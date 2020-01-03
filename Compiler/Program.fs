// Learn more about F# at http://fsharp.org

open System
open Core
open StgGen
open Ast

let astModule : Program<Var> =
    [
        TypeDecl(builtInVar IntegerConstr, [localVar "i"])
        GlobalDecl((globalVar "add"), 
            Block([localVar "x_boxed"; localVar "y_boxed"],
                [Return(
                    Match(
                        Var((localVar "x_boxed")),
                        [
                            PatConstr(builtInVar IntegerConstr, [PatBind (localVar "x")]),
                            Match(
                                Var((localVar "y_boxed")),
                                [
                                    PatConstr(builtInVar IntegerConstr, [PatBind (localVar "y")]),
                                    Match(
                                        Prim [PrimWasm Wasm.I32Add; PrimVar (localVar "x"); PrimVar (localVar "y")],
                                        [
                                            PatBind(localVar "r"), (Call (builtInVar IntegerConstr, [Var (localVar "r")]))
                                        ]
                                    )
                                ]
                            )
                        ]
                    )
                )]
            )
        )
        GlobalDecl((globalVar "subtract"), 
            Block([localVar "x_boxed"; localVar "y_boxed"],
                [Return(
                    Match(
                        Var((localVar "x_boxed")),
                        [
                            PatConstr(builtInVar IntegerConstr, [PatBind (localVar "x")]),
                            Match(
                                Var((localVar "y_boxed")),
                                [
                                    PatConstr(builtInVar IntegerConstr, [PatBind (localVar "y")]),
                                    Match(
                                        Prim [PrimWasm Wasm.I32Sub; PrimVar (localVar "y"); PrimVar (localVar "x")],
                                        [
                                            PatBind(localVar "r"),
                                            Call (builtInVar IntegerConstr, [Var (localVar "r")])
                                        ]
                                    )
                                ]
                            )
                        ]
                    )
                )]
            )
        )
        GlobalDecl((globalVar "fibonacci"), 
            Block([localVar "x"],
                [
                Assign(localVar "x_boxed", Call(builtInVar IntegerConstr, [Var (localVar "x")]))
                Return(
                    Match(
                        Call(globalVar "fibonacci_boxed", [Var(localVar "x_boxed")]),
                        [
                            PatConstr(builtInVar IntegerConstr, [PatBind (localVar "result")]),
                            Var((localVar "result"))
                        ]
                    )
                )
                ]
            )
        )
        GlobalDecl((globalVar "fibonacci_boxed"), 
            Block([localVar "x_boxed"],
                [
                    Return(
                        Match(
                            Var(localVar "x_boxed"),
                            [
                                PatLit(Box (Integer 0)), Lit(Box (Integer 1))
                                PatLit(Box (Integer 1)), Lit(Box (Integer 1))
                                PatBind(localVar "x_boxed_eval"), 
                                Call((globalVar "add"),
                                    [
                                    Call(globalVar "fibonacci_boxed",
                                        [
                                        Call(globalVar "subtract",
                                            [
                                                Var(localVar "x_boxed_eval")
                                                Lit(Box (Integer 1))
                                            ])
                                        ])
                                    Call(globalVar "fibonacci_boxed",
                                        [
                                        Call(globalVar "subtract",
                                            [
                                                Var(localVar "x_boxed_eval")
                                                Lit(Box (Integer 1))
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
