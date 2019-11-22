module WasmGen

open Stg

type Var =
    | UserVar of string
    | Identity
    | Malloc
    | This
    | ThisFunction
    | NextArgPtr

type Placement<'b> =
    | Heap of uint32
    | Local of Wasm.LocalIdx
    | Func of Wasm.FuncIdx
    | IndirectFunc of Wasm.TableIdx
    | Lambda of Wasm.LocalIdx * Wasm.FuncIdx * Args<'b> * Free<'b>
    | Unreachable

type RuntimeFunction =
    { name: Var
      functype: Wasm.FuncType
      indirect: bool
      func: Wasm.Func }

type Func =
    { name: Var
      functype: Wasm.FuncType
      indirect: bool }

let stdFuncType = [ Wasm.I32 ], [ Wasm.I32 ]
let heapTop = 0u


let getLocal env v =
    match env |> Map.find v with
    | Local local -> local
    | _ -> failwith "Error"

let getFunc env v =
    match env |> Map.find v with
    | Func func -> func
    | _ -> failwith "Error"

let getIndirectFunc env v =
    match env |> Map.find v with
    | IndirectFunc func -> func
    | _ -> failwith "Error"

let getLambda env v =
    match env |> Map.find v with
    | Lambda(localidx, funcidx, args, frees) -> (localidx, funcidx, args, frees)
    | _ -> failwith "Error"

let genAtom tenv env =
    function
    | AVar v ->
        match env |> Map.tryFind v with
        | Some(Heap i) ->
            [ Wasm.LocalGet 0u
              Wasm.I32Load
                  { align = 0u
                    offset = 4u * uint32 i } ]
        | Some(Local i) -> [ Wasm.LocalGet i ]
        | Some(Func i) -> [ Wasm.Call i ]
        | Some(Lambda(i, _, _, _)) -> [ Wasm.LocalGet i ]
        | Some(Unreachable) -> failwith "Error"
        | None -> failwith "Error"
    | ALit w -> [ w ]


let rec genExpr tenv env =
    function
    | Let(binds, e) -> genLet tenv env binds e
    | Case(e, v, alts) -> genCase tenv env v e alts
    | App(v, []) -> genApp tenv env v
    | App(v, atoms) -> failwith "Not Implemented"
    | Constr(constr, atoms) -> genPrim tenv env (AVar constr :: atoms)
    | Prim(atoms) -> genPrim tenv env atoms

and genLet tenv env binds e =
    [ genBinds tenv env binds
      genExpr tenv env e ]
    |> List.concat

and genBinds tenv env = function
    | NonRec [ x ] ->
        let (localidx, funcidx, args, frees) = getLambda env x
        let size = 1 + List.length args + List.length frees
        let freeOffset = 1 + List.length args
        let storeFrees = (frees |> List.mapi(fun i free -> 
            [
                [Wasm.LocalGet localidx]
                genAtom tenv env (AVar free)
                [Wasm.I32Store
                  { align = 0u
                    offset = 4u * uint32 (freeOffset+i) }]
            ]) |> List.concat |> List.concat)
        [
            [ 
                Wasm.I32Const (4 * size)
                Wasm.Call(getFunc env Malloc)
                Wasm.LocalSet localidx
                Wasm.LocalGet localidx
                Wasm.I32Const(int32 funcidx)
                Wasm.I32Store
                  { align = 0u
                    offset = 0u } 
            ]
            storeFrees
        ] |> List.concat

and genApp tenv env v =
    [ genAtom tenv env (AVar v)
      genAtom tenv env (AVar v)
      [ Wasm.I32Load
          { align = 0u
            offset = 0u } ]
      [ Wasm.CallIndirect(tenv |> Map.find stdFuncType) ] ]
    |> List.concat

and genCase tenv env (v: Var) e alts =
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
        ([ genAtom tenv env atom
           [ lit
             Wasm.I32Eq
             Wasm.IfElse([ Wasm.I32 ], genExpr tenv env e, genPAlts tenv env atom def xs) ] ]
         |> List.concat)
    | [] -> genExpr tenv env def

and genAAlts tenv env atom def =
    function
    | ((c, vs), e) :: xs ->
        let funcIndex = getFunc env c

        let assignLocals =
            vs
            |> List.mapi (fun i v ->
                let local = getLocal env v
                [ genAtom tenv env atom
                  [ Wasm.I32Load
                      { align = 0u
                        offset = 4u * (2u + uint32 (i)) } ]
                  [ Wasm.LocalSet local ] ]
                |> List.concat)

        [ genAtom tenv env atom
          [ Wasm.I32Load
              { align = 0u
                offset = 4u * 1u }
            Wasm.I32Const(int funcIndex)
            Wasm.I32Eq
            Wasm.IfElse
                ([ Wasm.I32 ],
                 [ assignLocals |> List.concat
                   genExpr tenv env e ]
                 |> List.concat, genAAlts tenv env atom def xs) ] ]
        |> List.concat
    | [] -> genExpr tenv env def

and genPrim tenv env atoms =
    atoms
    |> List.rev
    |> List.collect (genAtom tenv env)


let rec genLetFuncs = function
    | Lifted(b, ((_, _, _, lets), _)) ->
        [ [ { name = b
              functype = stdFuncType
              indirect = true } ]
          genLetsFuncs lets ]
        |> List.concat

and genLetsFuncs = List.collect genLetFuncs

let genTopLamFuncs b ((vars, e): LambdaForm<_>) =
    let (args, free, locals, lets) = vars
    [ [ { name = b
          functype = (Wasm.I32 |> List.replicate (args |> List.length), [ Wasm.I32 ])
          indirect = false } ]
      genLetsFuncs lets ]
    |> List.concat

let genTopConstrFunc b vs =
    { name = b
      functype = (Wasm.I32 |> List.replicate (vs |> List.length), [ Wasm.I32 ])
      indirect = false }

let genTopLevelFuncs =
    function
    | b, TopLam lam -> genTopLamFuncs b lam
    | b, TopConstr vs -> [ genTopConstrFunc b vs ]

let placeLocal local i = (local, Local i)
let placeLet env (Lifted(b, ((args, free, _, _), _): LambdaForm<_>)) i =
    (b, Lambda(i, getIndirectFunc env b, args, free))

let localsEnv env locals lets =
    let wasmLocals =
        List.concat
            [ This :: locals |> List.map placeLocal
              lets |> List.map (placeLet env) ]
    Seq.initInfinite (uint32)
    |> Seq.zip wasmLocals
    |> Seq.map (fun (f, i) -> f i)

let rec genLetCode tenv env = function
    | Lifted(_, (vars, code): LambdaForm<_>) ->
        let (args, free, locals, lets) = vars

        let newEnv =
            Seq.concat
                [ Map.toSeq env
                  localsEnv env locals lets
                  Seq.initInfinite (uint32 >> Heap)
                  |> Seq.zip (ThisFunction :: NextArgPtr :: List.concat [ args; free ]) ]
            |> Map.ofSeq

        [ [ Wasm.I32 |> List.replicate ((locals |> List.length) + (lets |> List.length)), genExpr tenv newEnv code ]
          genLetsCode tenv env lets ]
        |> List.concat

and genLetsCode tenv env = List.collect (genLetCode tenv env)

let genTopBindCode tenv env ((vars, code): LambdaForm<_>) =
    let (args, free, locals, lets) = vars
    if not (List.isEmpty free) then
        failwith "Error"
    else
        let newEnv =
            Seq.concat
                [ Map.toSeq env
                  localsEnv env locals lets
                  Seq.initInfinite (uint32 >> Local) |> Seq.zip (List.concat [ args; locals ]) ]
            |> Map.ofSeq

        [ [ Wasm.I32 |> List.replicate ((locals |> List.length) + (lets |> List.length)), genExpr tenv newEnv code ]
          genLetsCode tenv env lets ]
        |> List.concat

let genTopConstrCode tenv env b vs =
    let localidx =
        vs
        |> List.length
        |> uint32

    let vsStores =
        vs
        |> List.mapi (fun i v ->
            [ 
              Wasm.LocalGet localidx
              Wasm.LocalGet(uint32 i)
              Wasm.I32Store
                  { align = 0u
                    offset = 4u * uint32 (i + 2)} ])

    [ Wasm.I32 ],
    [ [ Wasm.I32Const
            (4 * (vs
             |> List.length
             |> (+) 2))
        Wasm.Call(getFunc env Malloc)
        Wasm.LocalSet localidx ]
      [ Wasm.LocalGet localidx
        Wasm.I32Const(getIndirectFunc env Identity |> int)        
        Wasm.I32Store
            { align = 0u
              offset = 4u * uint32 (0u) } ]
      [ 
        Wasm.LocalGet localidx
        Wasm.I32Const(getFunc env b |> int)
        Wasm.I32Store
            { align = 0u
              offset = 4u * uint32 (1u) } ]
      vsStores |> List.concat
      [ Wasm.LocalGet localidx ] ]
    |> List.concat

let genTopLevelCode tenv env =
    function
    | _, TopLam lam -> genTopBindCode tenv env lam
    | b, TopConstr(vs) -> [ genTopConstrCode tenv env b vs ]


let identity =
    { name = Identity
      functype = stdFuncType
      indirect = true
      func = [], [ Wasm.LocalGet 0u ] }

(*/ Memory layout:
-1: Size
0: Function
1: Constr
...: Data
*)
let malloc =
    { name = Malloc
      functype = [ Wasm.I32 ], [ Wasm.I32 ]
      indirect = false
      func =
          [],
          [ [
              
              // Load heapTop + 4
              Wasm.GlobalGet heapTop
              Wasm.I32Const 4
              Wasm.I32Add ]

            // Save heapTop
            [ Wasm.GlobalGet heapTop ]

            // Increment to save size at -1
            [ Wasm.GlobalGet heapTop
              Wasm.LocalGet 0u
              Wasm.I32Const 4
              Wasm.I32Add
              Wasm.I32Add
              Wasm.GlobalSet heapTop ]

            // Grow if needed
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

            // Load heapTop
            [ Wasm.LocalGet 0u
              Wasm.I32Store
                  { align = 0u
                    offset = 0u } ] 

            // Return (old) heapTop + 4          
          ]
          |> List.concat }


let genProgram (program: Program<_>) =
    let runtimeFuncs = [ identity; malloc ]

    let topLevelFuncs =
        List.concat
            [ runtimeFuncs
              |> List.map (fun x ->
                  { name = x.name
                    functype = x.functype
                    indirect = x.indirect })
              program |> List.collect (genTopLevelFuncs) ]

    let typeSec =
        topLevelFuncs
        |> List.map (fun x -> x.functype)
        |> List.distinct

    let tenv =
        Seq.initInfinite (uint32)
        |> Seq.zip (typeSec)
        |> Seq.toList
        |> Map.ofSeq

    let funcSec = topLevelFuncs |> List.map (fun x -> tenv |> Map.find x.functype)

    let topLevelFuncVars = topLevelFuncs |> List.map (fun x -> x.name)

    let exportSec =
        topLevelFuncVars
        |> List.indexed
        |> List.choose (function
            | (i, UserVar nm) ->
                Some
                    { Wasm.Export.nm = nm
                      Wasm.Export.exportdesc = Wasm.ExportFunc(uint32 i) }
            | _ -> None)


    let indirectFuncs = topLevelFuncs |> List.filter (fun x -> x.indirect)

    let indirectFuncsVars = indirectFuncs |> List.map (fun x -> x.name)

    let funcenv =
        Seq.initInfinite (uint32 >> Func)
        |> Seq.zip (topLevelFuncVars)
        |> Map.ofSeq

    let indirectElems = indirectFuncs |> List.map (fun x -> getFunc funcenv x.name)

    let indirectElemsLen =
        indirectElems
        |> List.length
        |> uint32

    let env =
        Seq.concat
            [ funcenv |> Map.toSeq
              Seq.initInfinite (uint32 >> IndirectFunc) |> Seq.zip (indirectFuncsVars) ]
        |> Map.ofSeq

    let codeSec =
        [ runtimeFuncs |> List.map (fun f -> f.func)
          program |> List.collect (genTopLevelCode tenv env) ]
        |> List.concat


    [ Wasm.TypeSec typeSec
      Wasm.FuncSec(funcSec)
      Wasm.TableSec [ Wasm.Table(Wasm.FuncRef, Wasm.MinMax(indirectElemsLen, indirectElemsLen)) ]
      Wasm.MemSec [ Wasm.Min 1u ]
      Wasm.GlobalSec
          [ { gt = Wasm.I32, Wasm.Var
              init = [ Wasm.I32Const 0 ] } ]
      Wasm.ExportSec(exportSec)
      Wasm.ElemSec
          [ { table = 0u
              offset = [ Wasm.I32Const 0 ]
              init = indirectElems } ]
      Wasm.CodeSec(codeSec) ]
