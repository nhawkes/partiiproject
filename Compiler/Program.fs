// Learn more about F# at http://fsharp.org

open System
open Core
open WasmGen
open StgGen

let coreModule : Program<Var> =
    [
        (globalVar "Int", TopConstr [localVar "I"])
        (globalVar "add"), TopExpr (         
            Lam(localVar "x_boxed", 
                Lam(localVar "y_boxed",
                    Case(
                        Var((localVar "x_boxed")),
                        (localVar "x_boxed_eval"),
                        Alts (                       
                            [  (Var(globalVar "Int"), [(localVar "x")]),
                                Case(
                                    Var((localVar "y_boxed")),
                                    (localVar "y_boxed_eval"),
                                    Alts (
                                        [(Var (globalVar "Int"), [(localVar "y")]),
                                            Case(
                                                Prim [Stg.ALit Wasm.I32Add; Stg.AVar (localVar "x"); Stg.AVar (localVar "y")],
                                                (localVar "r"),
                                                Alts([], 
                                                    App (Var(globalVar "Int"), Var (localVar "r"))))                                    
                                        ],
                                        Prim [Stg.ALit Wasm.Unreachable]
                                    )
                                )
                            ],
                            Prim [Stg.ALit Wasm.Unreachable]
                        )
                    )
                )
            )
        )
        (globalVar "subtract"), TopExpr (    
            Lam(localVar "x_boxed", 
                Lam(localVar "y_boxed",
                    Case(
                        Var (localVar "x_boxed"),
                        (localVar "x_boxed_eval"),
                        Alts (
                            [(Var (globalVar "Int"),[(localVar "x")]),
                                Case(
                                    Var((localVar "y_boxed")),
                                    (localVar "y_boxed_eval"),
                                    Alts (
                                        [(Var (globalVar "Int"),[(localVar "y")]),
                                            Case(
                                                Prim [Stg.ALit Wasm.I32Sub; Stg.AVar (localVar "y"); Stg.AVar (localVar "x");],
                                                (localVar "r"),
                                                Alts([], 
                                                    App (Var (globalVar "Int"), Var (localVar "r"))))                                    
                                        ],
                                        Prim [Stg.ALit Wasm.Unreachable]
                                    )
                                )
                            ],
                            Prim [Stg.ALit Wasm.Unreachable]
                        )
                    )     
                )
            )
        )
        (globalVar "fibonacci"),  TopExpr (   
            Lam(localVar "x", 
                Let(NonRec [localVar "x_boxed", App(Var (globalVar "Int"), Var (localVar "x"))], 
                    Case(
                        Prim[Stg.AVar (globalVar "fibonacci_boxed");  Stg.AVar((localVar "x_boxed"))],
                        (localVar "result"),
                        Alts(
                            [(Var (globalVar "Int"), [(localVar "return")]),
                                Prim [Stg.AVar (localVar "return")]
                            ],
                            Prim [Stg.ALit Wasm.Unreachable]
                        )
                    
                    )       
                )  
            )       
        )
        (globalVar "fibonacci_boxed"), TopExpr( 
            Lam(localVar "x_boxed", 
                Case(
                    Var(localVar "x_boxed"),
                    (localVar "x_boxed_eval"),
                    Alts(
                        [(Var (globalVar "Int"), [(localVar "x")]),
                            Case(
                                Prim[Stg.AVar (localVar "x")], 
                                (localVar "x_eval"),
                                Alts (
                                    [
                                        (Prim [Stg.ALit (Wasm.I32Const 0)], []), App (Var (globalVar "Int"), Prim [Stg.ALit (Wasm.I32Const 1)])
                                        (Prim [Stg.ALit (Wasm.I32Const 1)], []), App (Var (globalVar "Int"), Prim [Stg.ALit (Wasm.I32Const 1)])
                                    ],
                                    App(
                                        App(
                                            Var(globalVar "add"),
                                            App(Var(globalVar "fibonacci_boxed"),
                                                App(App(Var(globalVar "subtract"), Var (localVar "x_boxed_eval")), App (Var (globalVar "Int"), Prim [Stg.ALit (Wasm.I32Const 1)])))
                                        ),
                                        App(Var(globalVar "fibonacci_boxed"),
                                                App(App(Var(globalVar "subtract"), Var (localVar "x_boxed_eval")), App (Var (globalVar "Int"), Prim [Stg.ALit (Wasm.I32Const 2)])))
                                    )                       
                                )
                            )
                        
                        ],
                        Prim [Stg.ALit Wasm.Unreachable]             
                    )
                )
            )
        )
    ]


[<EntryPoint>]
let main argv =
    let stgModule = 
        coreModule
         |> StgGen.genProgram   
    let wasmModule =
        stgModule          
         |> WasmGen.genProgram
    let bytes = Emit.emitWasmModule wasmModule |> List.toArray
    IO.File.WriteAllBytes("./Compiler.Benchmark/out/wasm/fibonacci.wasm", bytes)
    0
