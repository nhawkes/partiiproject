module WasmGen

open Stg

type Var =
    | UserVar of string
    | Malloc

type Placement =
    | Free of uint32
    | Local of uint32
    | Func of uint32
    | Unreachable

type RuntimeFunction =
    { name: Var
      functype: Wasm.FuncType
      func: Wasm.Func }

let stdFuncType = [ Wasm.I32 ], [ Wasm.I32 ]
let heapTop = 0u

let genAtom tenv env =
    function
    | AVar v ->
        match env |> Map.tryFind v with
        | Some(Free i) -> failwith "Not Implemented"
        | Some(Local i) -> Wasm.LocalGet i
        | Some(Func i) -> Wasm.Call i
        | Some(Unreachable) -> failwith "Error"
        | None -> failwith "Error"
    | ALit w -> w


let rec genExpr tenv env =
    function
    | Let(binds, e) -> failwith "Not Implemented"
    | Case(e, v, alts) -> genCase tenv env v e alts
    | App(v, []) -> genApp tenv env v
    | App(v, atoms) -> failwith "Not Implemented"
    | Constr(constr, atoms) -> failwith "Not Implemented"
    | Prim(atoms) -> genPrim tenv env atoms

and genApp tenv env v =
    [ genAtom tenv env (AVar v)
      genAtom tenv env (AVar v)
      Wasm.I32Load
          { align = 0u
            offset = 0u }
      Wasm.CallIndirect (tenv |> Map.find stdFuncType)]

and getLocal env v =
    match env |> Map.find v with
    | Local local -> local
    | _ -> failwith "Error"
    
and getFunc env v =
    match env |> Map.find v with
    | Func func -> func
    | _ -> failwith "Error"

and genCase tenv env (v:Var) e alts =
    let atom = AVar v
    let local = getLocal env v
    [ e |> genExpr tenv env
      [ Wasm.LocalSet(local) ]
      alts |> genAlts tenv env atom ]
    |> List.concat

and genAlts tenv env atom =
    function
    | AAlts(aalts, def) -> genAAlts tenv env atom def aalts
    | PAlts(palts, def) -> genPAlts tenv env atom def palts

and genPAlts tenv env atom def =
    function
    | (lit, e) :: xs ->
        [ genAtom tenv env atom
          lit
          Wasm.I32Eq
          Wasm.IfElse([ Wasm.I32 ], genExpr tenv env e, genPAlts tenv env atom def xs) ]
    | [] -> genExpr tenv env def

and genAAlts tenv env atom def =
    function
    | ((c, vs), e) :: xs ->
        let funcIndex = getFunc env c
        let assignLocals = vs |> List.mapi (fun i v ->
            let local = getLocal env v
            [
                genAtom tenv env atom
                Wasm.I32Load { align = 0u; offset = 2u+uint32(i) }
                Wasm.LocalSet local
            ]) 

        [ genAtom tenv env atom
          Wasm.I32Load { align = 0u; offset = 1u }
          Wasm.I32Const (int funcIndex)
          Wasm.I32Eq
          Wasm.IfElse(
              [ Wasm.I32 ], 
              [
                  assignLocals |> List.concat
                  genExpr tenv env e
              ] |> List.concat, 
              genAAlts tenv env atom def xs) ]
    | [] -> genExpr tenv env def

and genPrim tenv env atoms =
    atoms
    |> List.rev
    |> List.map (genAtom tenv env)

let genTopLamType ((vars, e): LambdaForm<_>) =
    let (args, free, locals) = vars
    Wasm.I32 |> List.replicate (args |> List.length), [ Wasm.I32 ] 

let genTopConstrType vs =
    Wasm.I32 |> List.replicate (vs |> List.length), [ Wasm.I32 ] 

let genTopLevelType = function
    |TopLam lam -> genTopLamType lam
    |TopConstr vs -> genTopConstrType vs

let genTopBind tenv env (vars, code) =
    let (args, free, locals) = vars
    if not (List.isEmpty free) then
        failwith "Error"
    else
        let newEnv =
            Seq.concat
                [ Map.toSeq env
                  Seq.initInfinite (uint32 >> Local) |> Seq.zip (List.concat [ args; locals ]) ]
            |> Map.ofSeq

        Wasm.I32 |> List.replicate (locals |> List.length), genExpr tenv newEnv code


let genTopLevelCode tenv env = function
    |TopLam lam -> genTopBind tenv env lam
    |TopConstr(vs) -> 
        [],
        [
            Wasm.I32Const (vs |> List.length |> (+) 1)
            Wasm.Call (getFunc env Malloc)
        ]


(*/ Memory layout:
-1: Size
0: Function
1: Constr
...: Data
*)
let malloc =
    { name = Malloc
      functype = [ Wasm.I32 ], [ Wasm.I32 ]
      func =
          [],
          [ [ Wasm.GlobalGet heapTop
              Wasm.I32Const 1
              Wasm.I32Add ]

            [ Wasm.GlobalGet heapTop ]

            [ Wasm.GlobalGet heapTop
              Wasm.LocalGet 0u
              Wasm.I32Const 1
              Wasm.I32Add
              Wasm.I32Add
              Wasm.GlobalSet heapTop ]

            [ Wasm.Loop
                ([],
                 [ Wasm.GlobalGet heapTop
                   Wasm.I32Const 16
                   Wasm.I32ShrU
                   Wasm.MemorySize
                   Wasm.I32GtU
                   Wasm.BrIf 0u
                   Wasm.I32Const 1
                   Wasm.MemoryGrow
                   Wasm.Drop ]) ]

            [ Wasm.LocalGet 0u
              Wasm.I32Store
                  { align = 0u
                    offset = 0u } ] ]
          |> List.concat }


let genProgram (program: Program<_>) =
    let runtimeFuncs = [ malloc ]


    let topLevelTypes =
        List.concat
            [ runtimeFuncs |> List.map (fun x -> x.functype)
              program |> List.map (snd >> genTopLevelType) ]

    let allTypes = stdFuncType :: topLevelTypes

    let tenv =
        Seq.initInfinite (uint32)
        |> Seq.zip (allTypes)
        |> Seq.toList
        |> Map.ofSeq

    let topLevelFuncVars =
        List.concat
            [ runtimeFuncs |> List.map (fun x -> x.name)
              program |> List.map fst ]


    let topLevelFuncs = topLevelTypes |> List.map (fun x -> tenv |> Map.find x)

    let topLevelExports =
        topLevelFuncVars
        |> List.indexed
        |> List.choose (function
            | (i, UserVar nm) ->
                Some
                    { Wasm.Export.nm = nm
                      Wasm.Export.exportdesc = Wasm.ExportFunc(uint32 i) }
            | _ -> None)

    let env =
        Seq.initInfinite (uint32 >> Func)
        |> Seq.zip (topLevelFuncVars)
        |> Seq.toList
        |> Map.ofSeq

    let topLevelCode = program |> List.map (snd >> genTopLevelCode tenv env)

    [ Wasm.TypeSec topLevelTypes
      Wasm.FuncSec topLevelFuncs
      Wasm.ExportSec topLevelExports
      Wasm.CodeSec topLevelCode ]
