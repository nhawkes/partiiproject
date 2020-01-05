module BuiltIns
open Ast
open Vars


type Name =
    |StringName of string
    |OpName of Op

let typeofBuiltIn = function
    | IntegerConstr -> TopFuncT([IntT], ValueT)


let builtInConstr builtInVar =
    let (TopFuncT(args, ValueT)) = builtInVar.typ
    let fields = args |> List.mapi(fun i t -> {unique=InternalField i;  name=""; typ=t})
    TypeDecl(builtInVar, fields)

let builtInOp builtInVar : Declaration<Var> = 
    GlobalDecl(builtInVar,
            [xValue; yValue], 
            Block [
                Return(
                    Match(
                        (Var((xValue)), ValueT),
                        [
                            PatConstr(integerConstrVar, [PatBind xInt]),
                            Match(
                                (Var((yValue)), ValueT),
                                [
                                    PatConstr(integerConstrVar, [PatBind yInt]),
                                    Match(
                                        (Prim [PrimWasm Wasm.I32Add; PrimVar xInt; PrimVar yInt], IntT),
                                        [
                                            PatBind(rInt), (Call (integerConstrVar, [Var (rInt)]))
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
        builtInConstr integerConstrVar
        builtInOp addOpVar
        builtInOp subOpVar
    ]

let builtInsEnv =
    [
        StringName "Int", integerConstrVar
        OpName Add, addOpVar
        OpName Sub, subOpVar
    ] |> Map.ofList
