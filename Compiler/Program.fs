// Learn more about F# at http://fsharp.org

open System
open Stg
open WasmGen

let stgModule : Program<Var> =
    [
        (UserVar "Int", TopConstr [UserVar "I"])
        (UserVar "add"), TopLam (([(UserVar "x_boxed");(UserVar "y_boxed")],[],[(UserVar "x");(UserVar "y");(UserVar "r")],[]),            
            Case(
                App((UserVar "x_boxed"), []),
                (UserVar "x_boxed"),
                AAlts (                       
                    [  (UserVar "Int", [(UserVar "x")]),
                        Case(
                            App((UserVar "y_boxed"), []),
                            (UserVar "x_boxed"),
                            AAlts (
                                [(UserVar "Int",[(UserVar "y")]),
                                    Case(
                                        Prim [ALit Wasm.I32Add; AVar (UserVar "x"); AVar (UserVar "y")],
                                        (UserVar "r"),
                                        PAlts([], 
                                            Constr (UserVar "Int", [AVar (UserVar "r")])))                                    
                                ],
                                Prim [ALit Wasm.Unreachable]
                            )
                        )
                    ],
                    Prim [ALit Wasm.Unreachable]
                )
            ) 
        )
        (UserVar "subtract"), TopLam (([(UserVar "x_boxed");(UserVar "y_boxed")],[],[(UserVar "x");(UserVar "y");(UserVar "r")],[]),
            Case(
                App((UserVar "x_boxed"), []),
                (UserVar "x_boxed"),
                AAlts (
                    [(UserVar "Int",[(UserVar "x")]),
                        Case(
                            App((UserVar "y_boxed"), []),
                            (UserVar "x_boxed"),
                            AAlts (
                                [(UserVar "Int",[(UserVar "y")]),
                                    Case(
                                        Prim [ALit Wasm.I32Sub; AVar (UserVar "x"); AVar (UserVar "y"); ],
                                        (UserVar "r"),
                                        PAlts([], 
                                            Constr (UserVar "Int", [AVar (UserVar "r")])))                                    
                                ],
                                Prim [ALit Wasm.Unreachable]
                            )
                        )
                    ],
                    Prim [ALit Wasm.Unreachable]
                )
            )            
        )
        (UserVar "fibonacci"),  TopLam (([(UserVar "x")],[],[(UserVar "result"); (UserVar "return")],[
            Lifted(((UserVar "x_boxed")), (([], [UserVar "x"], [], []), Constr(UserVar "Int", [AVar (UserVar "x")])))
        ]),
            Let(NonRec [UserVar "x_boxed"], 
                Case(
                    Prim[AVar (UserVar "fibonacci_boxed");  AVar((UserVar "x_boxed"))],
                    (UserVar "result"),
                    AAlts(
                        [(UserVar "Int", [(UserVar "return")]),
                            Prim [AVar (UserVar "return")]
                        ],
                        Prim [ALit Wasm.Unreachable]
                    )
                
                )       
            )        
        )
        (UserVar "fibonacci_boxed"), TopLam (([(UserVar "x_boxed")],[],[(UserVar "one"); (UserVar "two"); (UserVar "x_minus_one");(UserVar "x_minus_two");(UserVar "a");(UserVar "b"); (UserVar "x")],[]),
            Case(
                App((UserVar "x_boxed"),[]),
                (UserVar "x_boxed"),
                AAlts(
                    [(UserVar "Int", [(UserVar "x")]),
                        Case(
                            Prim[AVar (UserVar "x")], 
                            (UserVar "x"),
                            PAlts (
                                [
                                    Wasm.I32Const 0, Constr (UserVar "Int", [ALit (Wasm.I32Const 1)])
                                    Wasm.I32Const 1, Constr (UserVar "Int", [ALit (Wasm.I32Const 1)])
                                ],
                                Case(
                                    Constr (UserVar "Int", [ALit (Wasm.I32Const 1)]),
                                    (UserVar "one"),
                                    PAlts(
                                        [],
                                        Case(
                                            Constr (UserVar "Int", [ALit (Wasm.I32Const 2)]),
                                            (UserVar "two"),
                                            PAlts(
                                                [],
                                                Case(
                                                    Prim[AVar(UserVar "subtract"); AVar (UserVar "x_boxed");  AVar (UserVar "one")],
                                                    (UserVar "x_minus_one"),
                                                    PAlts(
                                                        [],
                                                        Case(
                                                            Prim[AVar(UserVar "subtract"); AVar (UserVar "x_boxed");  AVar (UserVar "two")],
                                                            (UserVar "x_minus_two"),
                                                            PAlts(
                                                                [],                                            
                                                                Case(
                                                                    Prim[AVar (UserVar "fibonacci_boxed"); AVar (UserVar "x_minus_one")],
                                                                    (UserVar "a"),
                                                                    PAlts(
                                                                        [],
                                                                        Case(
                                                                            Prim[AVar (UserVar "fibonacci_boxed");  AVar (UserVar "x_minus_two")],
                                                                            (UserVar "b"),
                                                                            PAlts([], 
                                                                                Prim[AVar(UserVar "add"); AVar (UserVar "a"); AVar (UserVar "b")])
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
                    Prim [ALit Wasm.Unreachable]             
                )
            )
        )
        
    ]


[<EntryPoint>]
let main argv =
    let wasmModule = stgModule |> WasmGen.genProgram
    let bytes = Emit.emitWasmModule wasmModule |> List.toArray
    IO.File.WriteAllBytes("./Compiler.Benchmark/out/wasm/fibonacci.wasm", bytes)
    0
