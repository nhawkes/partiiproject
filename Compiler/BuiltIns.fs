module BuiltIns
open Vars
open Types
open Core

let v s = Internal s, 0


let builtInOp builtInVar w = 
    builtInVar, (NoExport,
        lam([v "x_boxed"],
            lam([v "y_boxed"],
                case(Var <| F (v "x_boxed"), v "x_boxed", 
                    [
                        DataAlt IntDestr, [v "x"],
                        case(Var <| F (v "y_boxed"), v "y_boxed", 
                            [
                                DataAlt IntDestr, [v "y"],
                                case(Prim(w, [Var(F <| v "x"); Var(F <| v "y")]), v "r", 
                                    [
                                        DefAlt, [], App(Var <| F (IntConstr, 0), [Var <| F (v "r")])
                                    ]
                                
                                )
                            ]
                        )
                    ]
                )
            )    
        )
    )

let builtInsConstrs =
    [
        (IntConstr, 0), (IntDestr, [v"x"])
    ]

let builtInExprs =
    [    
        builtInOp (AddOp, 0) Wasm.I32Add
        builtInOp (SubOp, 0) Wasm.I32Sub
    ]
