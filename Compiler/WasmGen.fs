module WasmGen

open Stg

type Placement =
    | Free of uint32
    | Local of uint32
    | Func of uint32
    | Unreachable



let genAtom env =
    function
    | AVar v ->
        match env |> Map.find v with
        | Free i -> failwith "Not Implemented"
        | Local i -> Wasm.LocalGet i
        | Func i -> Wasm.Call i
        | Unreachable -> failwith "Error"
    | ALit w -> w

let rec genExpr env =
    function
    | Let(binds, e) -> failwith "Not Implemented"
    | Case(e, v, alts) -> genCase env v e alts
    | App(v, atoms) -> failwith "Not Implemented"
    | Constr(constr, atoms) -> failwith "Not Implemented"
    | Prim(atoms) -> genPrim env atoms

and genCase env v e alts =
    let atom = AVar v
    match env |> Map.find v with
    | Local i ->
        [ e |> genExpr env
          [ Wasm.LocalSet(i) ]
          alts |> genAlts env atom ]
        |> List.concat
    | _ -> failwith "Error"

and genAlts env atom =
    function
    | AAlts(aalts, def) -> failwith "Not Implemented"
    | PAlts(palts, def) -> genPAlts env atom def palts

and genPAlts env atom def =
    function
    | (lit, e) :: xs ->
        [ genAtom env atom
          lit
          Wasm.I32Eq
          Wasm.IfElse([ Wasm.I32 ], genExpr env e, genPAlts env atom def xs) ]
    | [] -> genExpr env def

and genPrim env atoms = atoms |> List.rev |> List.map (genAtom env)

let genTopLevelType ((vars, e): LambdaForm) =
    let (args, free, locals) = vars
    Wasm.I32 |> List.replicate (args |> List.length), [ Wasm.I32 ]

let genTopLevelCode env ((vars, e): LambdaForm): Wasm.Code =
    let (args, free, locals) = vars
    if not (List.isEmpty free) then
        failwith "Error"
    else
        let newEnv =
            Seq.concat
                [ Map.toSeq env
                  Seq.initInfinite (uint32 >> Local) |> Seq.zip (List.concat [args;locals])]
            |> Map.ofSeq

        Wasm.I32 |> List.replicate (locals |> List.length), genExpr newEnv e


let genProgram (program: Program) =
    let topLevelFuncVars = program |> List.map fst
    let topLevelTypes = program |> List.map (snd >> genTopLevelType)

    let topLevelFuncs =
        Seq.initInfinite uint32
        |> Seq.take (topLevelTypes |> List.length)
        |> Seq.toList

    let topLevelExports =
        topLevelFuncVars
        |> List.indexed
        |> List.map (fun (i, nm) ->
            { Wasm.Export.nm = nm
              Wasm.Export.exportdesc = Wasm.ExportFunc(uint32 i) })

    let env =
        Seq.initInfinite (uint32 >> Func)
        |> Seq.zip (topLevelFuncVars)
        |> Seq.toList
        |> Map.ofSeq

    let topLevelCode = program |> List.map (snd >> genTopLevelCode env)

    [ Wasm.TypeSec topLevelTypes
      Wasm.FuncSec topLevelFuncs
      Wasm.ExportSec topLevelExports
      Wasm.CodeSec topLevelCode ]
