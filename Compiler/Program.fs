// Learn more about F# at http://fsharp.org

open System
open Core
open WasmGen
open StgGen

let coreModule : Program<Var> =
    [
        (topVar "Int", TopConstr [var "I"])
        (topVar "add"), TopExpr (         
            Lam(var "x_boxed", 
                Lam(var "y_boxed",
                    Case(
                        Var((var "x_boxed")),
                        (var "x_boxed_eval"),
                        Alts (                       
                            [  (Var(topVar "Int"), [(var "x")]),
                                Case(
                                    Var((var "y_boxed")),
                                    (var "y_boxed_eval"),
                                    Alts (
                                        [(Var (topVar "Int"), [(var "y")]),
                                            Case(
                                                Prim [Stg.ALit Wasm.I32Add; Stg.AVar (var "x"); Stg.AVar (var "y")],
                                                (var "r"),
                                                Alts([], 
                                                    App (Var(topVar "Int"), Var (var "r"))))                                    
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
        (topVar "subtract"), TopExpr (    
            Lam(var "x_boxed", 
                Lam(var "y_boxed",  
                    Case(
                        Var (var "x_boxed"),
                        (var "x_boxed_eval"),
                        Alts (
                            [(Var (topVar "Int"),[(var "x")]),
                                Case(
                                    Var((var "y_boxed")),
                                    (var "y_boxed_eval"),
                                    Alts (
                                        [(Var (topVar "Int"),[(var "y")]),
                                            Case(
                                                Prim [Stg.ALit Wasm.I32Sub; Stg.AVar (var "y"); Stg.AVar (var "x");],
                                                (var "r"),
                                                Alts([], 
                                                    App (Var (topVar "Int"), Var (var "r"))))                                    
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
        (topVar "fibonacci"),  TopExpr (   
            Lam(var "x", 
                Let(NonRec [var "x_boxed", App(Var (topVar "Int"), Var (var "x"))], 
                    Case(
                        Prim[Stg.AVar (topVar "fibonacci_boxed");  Stg.AVar((var "x_boxed"))],
                        (var "result"),
                        Alts(
                            [(Var (topVar "Int"), [(var "return")]),
                                Prim [Stg.AVar (var "return")]
                            ],
                            Prim [Stg.ALit Wasm.Unreachable]
                        )
                    
                    )       
                )  
            )       
        )
        (topVar "fibonacci_boxed"), TopExpr( 
            Lam(var "x_boxed", 
                Case(
                    Var(var "x_boxed"),
                    (var "x_boxed_eval"),
                    Alts(
                        [(Var (topVar "Int"), [(var "x")]),
                            Case(
                                Prim[Stg.AVar (var "x")], 
                                (var "x_eval"),
                                Alts (
                                    [
                                        (Prim [Stg.ALit (Wasm.I32Const 0)], []), App (Var (topVar "Int"), Prim [Stg.ALit (Wasm.I32Const 1)])
                                        (Prim [Stg.ALit (Wasm.I32Const 1)], []), App (Var (topVar "Int"), Prim [Stg.ALit (Wasm.I32Const 1)])
                                    ],
                                    Case(
                                        App(App(Var(topVar "subtract"), Var (var "x_boxed_eval")),  App (Var (topVar "Int"), Prim [Stg.ALit (Wasm.I32Const 1)])),
                                        (var "x_minus_one"),
                                        Alts(
                                            [],
                                            Case(
                                                App(App(Var(topVar "subtract"), Var (var "x_boxed_eval")), App (Var (topVar "Int"), Prim [Stg.ALit (Wasm.I32Const 2)])),
                                                (var "x_minus_two"),
                                                Alts(
                                                    [],                            
                                                    Case(
                                                        Prim[Stg.AVar (topVar "fibonacci_boxed"); Stg.AVar (var "x_minus_one")],
                                                        (var "a"),
                                                        Alts(
                                                            [],
                                                            Case(
                                                                Prim[Stg.AVar (topVar "fibonacci_boxed");  Stg.AVar (var "x_minus_two")],
                                                                (var "b"),
                                                                Alts([], 
                                                                    Prim[Stg.AVar(topVar "add"); Stg.AVar (var "a"); Stg.AVar (var "b")]
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
