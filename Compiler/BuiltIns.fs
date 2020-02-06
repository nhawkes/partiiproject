module BuiltIns
open Vars
open Types
open Core

let v s = F (s2n s, {v=None; typ=ValueT; hintInline=false})
let inline b<'a> s :Binder<Vars.Var> = s2n s, {v=None; typ=ValueT; hintInline=false}
let builtInOp<'a when 'a:comparison> (builtInVar) w = 
    (builtInVar:Core.Binder<_>), (NoExport,
        (lamE(b "x_boxed",
            lamE(b "y_boxed",
                caseE(Var(v "x_boxed"), b "x_boxed", 
                    [
                        DataAlt IntDestr, [b "x"],
                        caseE(Var(v "y_boxed"), b "y_boxed", 
                            [
                                DataAlt IntDestr, [b "y"],
                                caseE(Prim(w, [Var(v "y"); Var(v "x")]), b "r", 
                                    [
                                        DefAlt, [], App(Var(F (intConstr)), Var <| v "r")
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
        intConstr, ((IntDestr), ["x", {v=None; typ=IntT; hintInline=false}])
    ]

let builtInExprs<'a when 'a:comparison> =
    [    
        builtInOp<'a> (addOp) Wasm.I32Add
        builtInOp (subOp) Wasm.I32Sub
    ]
