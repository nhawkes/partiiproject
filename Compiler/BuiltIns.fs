module BuiltIns
open Ast
open Vars


let typeofBuiltIn = function
    | IntegerConstr -> TopFuncT([IntT], ValueT)


let builtInConstr builtInVar =
    let (TopFuncT(args, ValueT)) = builtInVar.typ
    let fields = args |> List.mapi(fun i t -> {unique=InternalField i;  name=""; typ=t; callArity=None})
    TypeDecl(builtInVar, fields)

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
                                        (Prim [PrimWasm w; PrimVar xInt; PrimVar yInt], IntT),
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

