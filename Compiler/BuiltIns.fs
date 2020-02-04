module BuiltIns
open Vars
open Types
open Core

let v s = F (s2n s)
let inline b<'a> s :Binder<Vars.Var> = {v=None; typ=ValueT}, s2n s
let builtInOp<'a when 'a:comparison> (builtInVar) w = 
    ({v=None; typ=FuncT(ValueT, FuncT(ValueT, ValueT))}, builtInVar), (NoExport,
        (lamE(b "x_boxed",
            lamE(b "y_boxed",
                caseE(Var(v "x_boxed"), b "x_boxed", 
                    [
                        DataAlt IntDestr, [b "x"],
                        caseE(Var(v "y_boxed"), b "y_boxed", 
                            [
                                DataAlt IntDestr, [b "y"],
                                caseE(Prim(w, [Var(v "x"); Var(v "y")]), b "r", 
                                    [
                                        DefAlt, [], App(Var(F intConstr), Var <| v "r")
                                    ]
                                
                                )
                            ]
                        )
                    ]
                )
            )    
        ):Expr<_>)
    )

let builtInConstrs<'a> =
    [
        ({v=None; typ=FuncT(IntT, ValueT)}, intConstr), ((IntDestr), ["x", {v=None; typ=IntT}])
    ]

let builtInExprs<'a when 'a:comparison> =
    [    
        builtInOp<'a> (addOp) Wasm.I32Add
        builtInOp (subOp) Wasm.I32Sub
    ]
