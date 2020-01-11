module WasmGen

open Stg
open RuntimeFunctions

(*
Memory layout:
All
======
-4: Size
0: ArgsRemaining
4: Function
...: Data/Thunk/Function

Func - ArgsRemaining>0
=======
8..n: args
n..: frees

Data - ArgsRemaining=0 Function=0
=======
8: Type Idx
12..: Fields

Thunk - ArgsRemaining=0 Function!=0
=======
8..: frees
*)



type Placement =
    | Heap of uint32
    | Local of Wasm.LocalIdx
    | Func of Wasm.FuncIdx
    | IndirectFunc of Wasm.TableIdx
    | Lambda of Wasm.LocalIdx * Wasm.FuncIdx * Args<Var> * Free<Var>
    | Data of uint32
    | Jump of uint32
    | Unreachable

type Func =
    { name: Var
      functype: Wasm.FuncType
      indirect: bool
      caf: bool }




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
        match env |> Map.tryFind (v) with
        | Some(Heap i) ->
            [ Wasm.LocalGet 0u
              Wasm.I32Load
                  { align = 0u
                    offset = 4u * uint32 i } ]
        | Some(Local i) -> [ Wasm.LocalGet i ]
        | Some(Func i) -> [ Wasm.Call i ]
        | Some(Lambda(i, _, _, _)) -> [ Wasm.LocalGet i ]
        | Some(Data(i)) -> [ Wasm.I32Const (int i) ]
        | Some(Unreachable) -> failwith "Error"
        | None -> failwithf "Variable %A not in environment" v
    | ALit w -> [ w ]

let genStgVarAtom tenv env = function
    |AVar v -> genAtom tenv env (AVar (StgVar v))
    |ALit w -> genAtom tenv env (ALit w)


let rec genExpr tenv (env:Map<Var, Placement>) =
    function
    | Let(binds, e) -> genLet tenv env binds e
    | Case(e, v, alts) -> genCase tenv env v e alts
    | App(v, atoms) -> genApp tenv env v atoms
    | Call(constr, atoms) -> genPrim tenv env (AVar constr :: (atoms |> List.rev))
    | Prim(atoms) -> genPrim tenv env atoms

and genLet tenv env binds e =
    [ genBinds tenv env (fun env -> genExpr tenv env e) binds
    ]
    |> List.concat

and genBinds tenv env e = function
    | NonRec x -> List.concat [x |> genNonRec tenv env; e env]
    | Rec xs -> 
        List.concat 
            [
                xs |> List.collect(genRecStart tenv env)
                xs |> List.collect(genRecEnd tenv env)
                e env
            ]  
    | Join (j, [], je) -> genLetJoin tenv env j je e 

and genNonRec tenv env  x =     
    let (localidx, funcidx, args, frees) = getLambda env (StgVar x)
    let size = 2 + List.length args + List.length frees
    let freeOffset = 2 + List.length args
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
            
            // Args remaining
            Wasm.LocalGet localidx
            Wasm.I32Const(4 * (args |> List.length))
            Wasm.I32Store
              { align = 0u
                offset = 0u }
                
            // Function
            Wasm.LocalGet localidx
            Wasm.I32Const(int32 funcidx)
            Wasm.I32Store
              { align = 0u
                offset = 4u}                 
           
        ]
        storeFrees
    ] |> List.concat

and genRecStart tenv env x =     
    let (localidx, funcidx, args, frees) = getLambda env (StgVar x)
    let size = 2 + List.length args + List.length frees
    [ 
        Wasm.I32Const (4 * size)
        Wasm.Call(getFunc env Malloc)
        Wasm.LocalSet localidx
        
        // Args remaining
        Wasm.LocalGet localidx
        Wasm.I32Const(4 * (args |> List.length))
        Wasm.I32Store
          { align = 0u
            offset = 0u }
            
        // Function
        Wasm.LocalGet localidx
        Wasm.I32Const(int32 funcidx)
        Wasm.I32Store
          { align = 0u
            offset = 4u}                 
       
    ]

and genRecEnd tenv env x =     
    let (localidx, funcidx, args, frees) = getLambda env (StgVar x)
    let freeOffset = 2 + List.length args
    let storeFrees = (frees |> List.mapi(fun i free -> 
        [
            [Wasm.LocalGet localidx]
            genAtom tenv env (AVar free)
            [Wasm.I32Store
              { align = 0u
                offset = 4u * uint32 (freeOffset+i) }]
        ]) |> List.concat |> List.concat)
    storeFrees

and genLetJoin tenv env j je e =
    let newEnv = env |> Map.add ( StgVar j ) (Jump 0u)
    [
        Wasm.Block([Wasm.I32], [
            [Wasm.Block([Wasm.I32], [
                ( e newEnv )
                [Wasm.Br 1u]
            ] |> List.concat)]
            genExpr tenv env je
        ] |> List.concat)
    ]

and genApp tenv env v args =
    let whnf =
        [            
            // Check if thunk and call if so

            genAtom tenv env (AVar (StgVar v))
            [Wasm.I32Load
              { align = 0u
                offset = 0u }]
            [Wasm.I32Eqz]
            // Args Remaining = 0

            genAtom tenv env (AVar (StgVar v))
            [Wasm.I32Load
              { align = 0u
                offset = 4u }]
            [Wasm.I32Eqz; Wasm.I32Const -1; Wasm.I32Xor]
            // Function != 0

            [Wasm.I32And]
            // Args Remaining = 0 and Function != 0 ie. is a thunk

            [
                Wasm.IfElse([Wasm.I32],
                    [ genAtom tenv env (AVar (StgVar v))
                      genAtom tenv env (AVar (StgVar v))
                      [ Wasm.I32Load
                          { align = 0u
                            offset = 4u } ]
                      [ Wasm.CallIndirect(tenv |> Map.find stdFuncType) ] ]
                    |> List.concat, [
                        genAtom tenv env (AVar (StgVar v))
                    ] |> List.concat
                )
            ]
        ] |> List.concat

    let clone = 
        if args |> List.isEmpty then
            []
        else 
            [Wasm.Call (getFunc env Clone);Wasm.Call (getFunc env Clone);Wasm.Call (getFunc env Clone);Wasm.Call (getFunc env Clone);Wasm.Call (getFunc env Clone);Wasm.Call (getFunc env Clone)]       

    let applyArgs =
            args |> List.collect(fun arg -> 
                [
                    genStgVarAtom tenv env arg
                    [Wasm.Call (getFunc env Apply)]
                    
                ] |> List.concat)
    
    [
        whnf
        clone
        applyArgs
    ] |> List.concat

and genCase tenv env (v: Vars.Var) e alts =
    let atom = AVar (StgVar v)
    let local = getLocal env (StgVar v)
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
        let funcIndex = getFunc env (StgVar c)

        let assignLocals =
            vs
            |> List.mapi (fun i v ->
                let local = getLocal env (StgVar v)
                [ genAtom tenv env atom
                  [ Wasm.I32Load
                      { align = 0u
                        offset = 4u * (3u + uint32 (i)) } ]
                  [ Wasm.LocalSet local ] ]
                |> List.concat)

        // This must a data heap object - check Type Idx
        [ genAtom tenv env atom
          [ Wasm.I32Load
              { align = 0u
                offset = 8u }
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
    |> List.collect (genStgVarAtom tenv env)


let rec genLetFuncs = function
    | (b, lf) ->
        [ [ { name = StgVar b
              functype = stdFuncType
              indirect = true
              caf = false } ]
          genLetsFuncs lf.lets ]
        |> List.concat

and genLetsFuncs = List.collect genLetFuncs

let genTopLamFuncs b (lf: LambdaForm<Vars.Var>) =
    [ [ { name = StgVar b
          functype = (Wasm.I32 |> List.replicate (lf.args |> List.length), [ Wasm.I32 ])
          indirect = false
          caf = false } ]
      genLetsFuncs lf.lets ]
    |> List.concat

let genTopCafFunc b lf =
    [
        [{
        name = StgVar b
        functype = stdFuncType
        indirect = true
        caf = true
        }]
        genLetsFuncs lf.lets
    ] |> List.concat 

let genTopConstrFunc b vs =
    { name = StgVar b
      functype = (Wasm.I32 |> List.replicate (vs |> List.length), [ Wasm.I32 ])
      indirect = false
      caf = false }

let genTopLevelFuncs =
    function
    | b:Vars.Var, TopLam lam -> genTopLamFuncs b lam
    | b, TopCaf lam -> genTopCafFunc b lam
    | b, TopConstr vs -> [ genTopConstrFunc b vs ]

let placeLocal local i = (local, Local i)
let placeLet (env:Map<Var, Placement>) ((b, lf: LambdaForm<Vars.Var>)) i =
    (StgVar b, Lambda(i, getIndirectFunc env (StgVar b), lf.args |> List.map StgVar, lf.frees |> List.map StgVar))

let localsEnv env args locals (lets:(Vars.Var*LambdaForm<Vars.Var>) list) =
    let wasmLocals =
        List.concat 
            [ args |> List.map (placeLocal)
              locals |> List.map (StgVar >> placeLocal)
              lets |> List.map (placeLet env) 
            ]
    Seq.initInfinite (uint32)
    |> Seq.zip wasmLocals
    |> Seq.map (fun (f, i) -> f i)

let rec genLetCode tenv env = function
    | (_, lf: LambdaForm<Vars.Var>) ->
        let newEnv =
            Seq.concat
                [ Map.toSeq env
                  localsEnv env [This] lf.locals lf.lets
                  Seq.initInfinite (uint32 >> Heap)
                  |> Seq.zip (ArgsRemaining :: ThisFunction :: List.concat [ lf.args |> List.rev |> List.map StgVar; lf.frees |> List.map StgVar]) ]
            |> Map.ofSeq

        [ [ Wasm.I32 |> List.replicate ((lf.locals |> List.length) + (lf.lets |> List.length)), genExpr tenv newEnv lf.expr ]
          genLetsCode tenv env lf.lets ]
        |> List.concat

and genLetsCode tenv env = List.collect (genLetCode tenv env)

let genTopBindCode tenv env (lf: LambdaForm<_>) =
    if not (List.isEmpty lf.frees) then
        failwithf "Top level bindings cannot have free variables: %A" lf.frees
    else
        let newEnv =
            Seq.concat
                [ Map.toSeq env
                  localsEnv env (lf.args |> List.map StgVar) lf.locals lf.lets 
                ]
            |> Map.ofSeq

        let wasmLocalTypes = Wasm.I32 |> List.replicate ((lf.locals |> List.length) + (lf.lets |> List.length))
        [ [wasmLocalTypes, genExpr tenv newEnv lf.expr ]
          genLetsCode tenv env lf.lets ]
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
                    offset = 4u * uint32 (i + 3)} ])

    [ Wasm.I32 ],
    [ [ Wasm.I32Const
            (4 * (vs
             |> List.length
             |> (+) 3))
        Wasm.Call(getFunc env Malloc)
        Wasm.LocalSet localidx ]

      // Create a data heap object

      // ArgsRemaining = 0
      [ Wasm.LocalGet localidx
        Wasm.I32Const(0)        
        Wasm.I32Store
            { align = 0u
              offset = 0u } ]

      // Function = Identity = 0
      [ Wasm.LocalGet localidx
        Wasm.I32Const(getIndirectFunc env Identity |> int)        
        Wasm.I32Store
            { align = 0u
              offset = 4u } ]

      [ 
        Wasm.LocalGet localidx
        Wasm.I32Const(getFunc env (StgVar b) |> int)
        Wasm.I32Store
            { align = 0u
              offset = 8u } ]
      vsStores |> List.concat
      [ Wasm.LocalGet localidx ] ]
    |> List.concat

let genTopLevelCode tenv env =
    function
    | _, TopLam lam -> genTopBindCode tenv env lam
    | _, TopCaf lam -> genTopBindCode tenv env lam
    | b, TopConstr(vs) -> [ genTopConstrCode tenv env b vs ]


let genCafData env (caf:Func) =
    let func = getIndirectFunc env caf.name
    [
        // Size = 12
        System.BitConverter.GetBytes(12u) |> Array.toList

        // ArgsRemaining = 0
        System.BitConverter.GetBytes(0u) |> Array.toList

        // Func 
        System.BitConverter.GetBytes(func : uint32) |> Array.toList

        // Reserved
        System.BitConverter.GetBytes(0u) |> Array.toList

    ] |> List.concat
    



let genProgram (program: Program<Vars.Var>) =
    let runtimeFuncs = [ identity; malloc; clone 1u; apply 0u ]

    let topLevelFuncs =
        List.concat
            [ runtimeFuncs
              |> List.map (fun x ->
                  { name = x.name
                    functype = x.functype
                    indirect = x.indirect
                    caf = false })
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
            | (i, StgVar {unique=Vars.Export nm}) ->
                Some
                    { Wasm.Export.nm = nm
                      Wasm.Export.exportdesc = Wasm.ExportFunc(uint32 i) }
            | _ -> None)


    let indirectFuncs = topLevelFuncs |> List.filter (fun x -> x.indirect)

    let indirectFuncsVars = indirectFuncs |> List.map (fun x -> x.name)

    let indirectFuncEnv = 
        Seq.initInfinite (uint32 >> IndirectFunc) |> Seq.zip (indirectFuncsVars) |> Map.ofSeq

    let funcenv =
        Seq.initInfinite (uint32 >> Func)
        |> Seq.zip (topLevelFuncVars)
        |> Map.ofSeq

    
    let cafs = topLevelFuncs |> List.filter(fun x -> x.caf)
    let dataInit = cafs |> List.collect (genCafData indirectFuncEnv) |> Array.ofList
    let cafenv =
        Seq.initInfinite (fun i -> uint32 (4+i*16) |> Data)
        |> Seq.zip (cafs |> List.map (fun x -> x.name))
        |> Map.ofSeq
    let heapStart = (cafs |> List.length) * 16

    let indirectElems = indirectFuncs |> List.map (fun x -> getFunc funcenv x.name)

    let indirectElemsLen =
        indirectElems
        |> List.length
        |> uint32

    let env =
        Seq.concat
            [ funcenv |> Map.toSeq
              indirectFuncEnv |> Map.toSeq
              cafenv |> Map.toSeq
            ]
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
              init = [ Wasm.I32Const heapStart ] } ]
      Wasm.ExportSec({ nm = "Memory"; exportdesc = Wasm.ExportMem 0u}::exportSec)
      Wasm.ElemSec
          [ { table = 0u
              offset = [ Wasm.I32Const 0 ]
              init = indirectElems } ]
      Wasm.CodeSec(codeSec) 
      Wasm.DataSec [{data=0u; offset=[Wasm.I32Const 0]; init=dataInit}]]
