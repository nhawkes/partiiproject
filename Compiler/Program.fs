// Learn more about F# at http://fsharp.org

open System
open Core
open WasmGen

let coreModule : Program<Var> =
    [
        (UserVar "Int", TopConstr [UserVar "I"])
        (UserVar "add"), TopExpr (         
            Lam(UserVar "x_boxed", 
                Lam(UserVar "y_boxed",
                    Case(
                        Var((UserVar "x_boxed")),
                        (UserVar "x_boxed_eval"),
                        Alts (                       
                            [  (Var(UserVar "Int"), [(UserVar "x")]),
                                Case(
                                    Var((UserVar "y_boxed")),
                                    (UserVar "y_boxed_eval"),
                                    Alts (
                                        [(Var (UserVar "Int"), [(UserVar "y")]),
                                            Case(
                                                Prim [Stg.ALit Wasm.I32Add; Stg.AVar (UserVar "x"); Stg.AVar (UserVar "y")],
                                                (UserVar "r"),
                                                Alts([], 
                                                    App (Var(UserVar "Int"), Var (UserVar "r"))))                                    
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
        (UserVar "subtract"), TopExpr (    
            Lam(UserVar "x_boxed", 
                Lam(UserVar "y_boxed",  
                    Case(
                        Var (UserVar "x_boxed"),
                        (UserVar "x_boxed_eval"),
                        Alts (
                            [(Var (UserVar "Int"),[(UserVar "x")]),
                                Case(
                                    Var((UserVar "y_boxed")),
                                    (UserVar "y_boxed_eval"),
                                    Alts (
                                        [(Var (UserVar "Int"),[(UserVar "y")]),
                                            Case(
                                                Prim [Stg.ALit Wasm.I32Sub; Stg.AVar (UserVar "x"); Stg.AVar (UserVar "y"); ],
                                                (UserVar "r"),
                                                Alts([], 
                                                    App (Var (UserVar "Int"), Var (UserVar "r"))))                                    
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
        (UserVar "fibonacci"),  TopExpr (   
            Lam(UserVar "x", 
                Let(NonRec [UserVar "x_boxed", App(Var (UserVar "Int"), Var (UserVar "x"))], 
                    Case(
                        Prim[Stg.AVar (UserVar "fibonacci_boxed");  Stg.AVar((UserVar "x_boxed"))],
                        (UserVar "result"),
                        Alts(
                            [(Var (UserVar "Int"), [(UserVar "return")]),
                                Prim [Stg.AVar (UserVar "return")]
                            ],
                            Prim [Stg.ALit Wasm.Unreachable]
                        )
                    
                    )       
                )  
            )       
        )
        (UserVar "fibonacci_boxed"), TopExpr( 
            Lam(UserVar "x_boxed", 
                Case(
                    Var(UserVar "x_boxed"),
                    (UserVar "x_boxed_eval"),
                    Alts(
                        [(Var (UserVar "Int"), [(UserVar "x")]),
                            Case(
                                Prim[Stg.AVar (UserVar "x")], 
                                (UserVar "x_eval"),
                                Alts (
                                    [
                                        (Prim [Stg.ALit (Wasm.I32Const 0)], []), App (Var (UserVar "Int"), Prim [Stg.ALit (Wasm.I32Const 1)])
                                        (Prim [Stg.ALit (Wasm.I32Const 1)], []), App (Var (UserVar "Int"), Prim [Stg.ALit (Wasm.I32Const 1)])
                                    ],
                                    Case(
                                        App (Var (UserVar "Int"), Prim [Stg.ALit (Wasm.I32Const 1)]),
                                        (UserVar "one"),
                                        Alts(
                                            [],
                                            Case(
                                                App (Var (UserVar "Int"), Prim [Stg.ALit (Wasm.I32Const 2)]),
                                                (UserVar "two"),
                                                Alts(
                                                    [],
                                                    Case(
                                                        Prim[Stg.AVar(UserVar "subtract"); Stg.AVar (UserVar "x_boxed_eval");  Stg.AVar (UserVar "one")],
                                                        (UserVar "x_minus_one"),
                                                        Alts(
                                                            [],
                                                            Case(
                                                                Prim[Stg.AVar(UserVar "subtract"); Stg.AVar (UserVar "x_boxed_eval");  Stg.AVar (UserVar "two")],
                                                                (UserVar "x_minus_two"),
                                                                Alts(
                                                                    [],                            
                                                                    Case(
                                                                        Prim[Stg.AVar (UserVar "fibonacci_boxed"); Stg.AVar (UserVar "x_minus_one")],
                                                                        (UserVar "a"),
                                                                        Alts(
                                                                            [],
                                                                            Case(
                                                                                Prim[Stg.AVar (UserVar "fibonacci_boxed");  Stg.AVar (UserVar "x_minus_two")],
                                                                                (UserVar "b"),
                                                                                Alts([], 
                                                                                    Prim[Stg.AVar(UserVar "add"); Stg.AVar (UserVar "a"); Stg.AVar (UserVar "b")]
                                                                                )
                                                                           )        
                                                                        )     
                                                                    )                                                   
                                                                )
                                                            )  
                                                        )         
                                                    )
                                                )
                                            )
                                        )
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
    let wasmModule = 
        coreModule
         |> StgGen.genProgram
         |> WasmGen.genProgram
    let bytes = Emit.emitWasmModule wasmModule |> List.toArray
    IO.File.WriteAllBytes("./Compiler.Benchmark/out/wasm/fibonacci.wasm", bytes)
    0
