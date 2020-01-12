module BuiltIns
open Ast
open Vars
open Types

let rec fieldsForType i = function
    |FuncT(_, t, b) -> 
        {unique=InternalField i;  name=""; typ=t; callType=None}::fieldsForType (i+1) b
    |ValueT -> []


let builtInConstr builtInVar =
    TypeDecl(builtInVar, fieldsForType 0 builtInVar.typ)

let builtInOp builtInVar w : Declaration<Var> = 
    GlobalDecl(builtInVar,
            [xValue; yValue],
            Block [
                Return(
                    Match(
                        (Var((xValue)), ValueT),
                        [
                            PatConstr(integerConstr, [PatBind xInt]),
                            Match(
                                (Var((yValue)), ValueT),
                                [
                                    PatConstr(integerConstr, [PatBind yInt]),
                                    Match(
                                        (Prim [PrimWasm w; PrimVar yInt; PrimVar xInt], IntT),
                                        [
                                            PatBind(rInt), (Call (integerConstr, [Var (rInt)]))
                                        ]
                                    )
                                ]
                            )
                        ]
                    )
                )
            ]
        )  

let builtIns =
    [
        builtInConstr integerConstr
        builtInOp addOp Wasm.I32Add
        builtInOp subOp Wasm.I32Sub
    ]

let builtInsEnv =
    [
        "Int", integerConstr
    ] |> Map.ofList

