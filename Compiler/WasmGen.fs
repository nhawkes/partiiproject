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
    | Lambda of Wasm.LocalIdx * Wasm.FuncIdx * Args * Free
    | StdConstr of Wasm.LocalIdx * Atom list
    | Caf of uint32 * Wasm.FuncIdx
    | Block of uint32
    | Unreachable

type Func =
    { name: Var
      functype: Wasm.FuncType
      indirect: bool
      caf: bool 
      export: string option      
    }

type Names = (Wasm.Name) list * ((Wasm.Name) list) list


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

let getBlock env depth v =
    match env |> Map.find v with
    | Block(i) -> [Wasm.Br (depth - i)]
    | _ -> failwith "Error"    

let rec genAtom tenv env depth =
    function
    | AVar v ->
        match env |> Map.tryFind (StgVar v) with
        | Some(Heap i) ->
            [ Wasm.LocalGet 0u
              Wasm.I32Load
                  { align = 0u
                    offset = 4u * uint32 i } ]
        | Some(Local i) -> [ Wasm.LocalGet i ]
        | Some(Func i) -> [ Wasm.Call i ]
        | Some(Lambda(i, _, _, _)) -> [ Wasm.LocalGet i ]
        | Some(Caf(i,_)) -> [ Wasm.I32Const (int i) ]
        | Some(StdConstr(i, atoms)) -> 
            [ 
                atoms |> List.collect (genStgVarAtom tenv env depth)  
                [Wasm.Call i] ]  |> List.concat
        | Some(Unreachable) -> failwith "Error"
        | None -> failwithf "Variable %A not in environment" v
    | ALit w -> [ w ]


and genStgVarAtom tenv env depth = function
    |AVar v -> genAtom tenv env depth (AVar (v))
    |ALit w -> genAtom tenv env depth (ALit w)


let rec genExpr tenv (env:Map<Var, Placement>) depth =
    function
    | Let(binds, e) -> genLet tenv env depth binds e
    | Case(e, v, alts) -> genCase tenv env depth v e alts
    | App(v, atoms) -> genApp tenv env depth v atoms
    | Call(f, atoms) -> genPrim tenv env depth (AVar f :: (atoms |> List.rev))
    | Constr(constr, atoms) -> genPrim tenv env depth (AVar constr :: (atoms |> List.rev))
    | Jump(j, atoms) -> genJump tenv env depth j atoms
    | Prim(atoms) -> genPrim tenv env depth atoms

and genLet tenv env depth binds e =
    [ genBinds tenv env depth (fun env depth -> genExpr tenv env depth e) binds
    ]
    |> List.concat

and genBinds tenv env depth e = function
    | NonRec x -> List.concat [x |> genNonRec tenv env depth; e env depth]
    | Rec xs -> 
        List.concat 
            [
                xs |> List.collect(genRecStart tenv env depth)
                xs |> List.collect(genRecEnd tenv env depth)
                e env depth
            ]  
    | Join (j, [arg], je) -> genLetJoin tenv env depth j arg je e 

and genNonRec tenv env depth  x =     
    let (localidx, funcidx, args, frees) = getLambda env (StgVar x)
    let size = 2 + List.length args + List.length frees
    let freeOffset = 2 + List.length args
    let storeFrees = (frees |> List.mapi(fun i free -> 
        [
            [Wasm.LocalGet localidx]
            genAtom tenv env depth (AVar free)
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

and genRecStart tenv env depth x =     
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

and genRecEnd tenv env depth x =     
    let (localidx, funcidx, args, frees) = getLambda env (StgVar x)
    let freeOffset = 2 + List.length args
    let storeFrees = (frees |> List.mapi(fun i free -> 
        [
            [Wasm.LocalGet localidx]
            genAtom tenv env depth (AVar free)
            [Wasm.I32Store
              { align = 0u
                offset = 4u * uint32 (freeOffset+i) }]
        ]) |> List.concat |> List.concat)
    storeFrees

and genLetJoin tenv env depth j arg je e =
    let newEnv = env |> Map.add ( StgVar j ) (Block (2u+depth))
    let jexpr = genExpr tenv env (depth + 1u) je
    let expr = ( e newEnv (depth + 2u))
    let result = [
        Wasm.Block([Wasm.I32], [
            [Wasm.Block([Wasm.I32], [
                expr
                [Wasm.Br 1u]
            ] |> List.concat)]
            [ Wasm.LocalSet (getLocal env (StgVar arg)) ]
            jexpr
        ] |> List.concat)
    ]
    result

and genApp tenv env depth v args =
    let whnf =
        [            
            genAtom tenv env depth (AVar (v))
            [Wasm.Call (getFunc env WhnfEval)]
        ] |> List.concat

    let clone = 
        if args |> List.isEmpty then
            []
        else 
            [Wasm.Call (getFunc env Clone)]       

    let applyArgs =
            args |> List.collect(fun arg -> 
                [
                    genStgVarAtom tenv env depth arg
                    [Wasm.Call (getFunc env Apply)]
                    
                ] |> List.concat)
    
    [
        whnf
        clone
        applyArgs
    ] |> List.concat

and genJump tenv env depth (j: Stg.Var) vs =
   match vs with
   | [arg] ->
       [
           genStgVarAtom tenv env depth arg
           getBlock env depth (StgVar j) 
       ] |> List.concat
    |_ -> failwith "Jumps only support one variable"

and genCase tenv env depth (v: Stg.Var) e alts =
    let atom = AVar (v)
    let local = getLocal env (StgVar v)
    [ e |> genExpr tenv env depth
      [ Wasm.LocalSet(local) ]
      alts |> genAlts tenv env depth atom ]
    |> List.concat

and genAlts tenv env depth atom =
    function
    | AAlts(aalts, def) -> genAAlts tenv env depth atom def aalts
    | PAlts(palts, def) -> genPAlts tenv env depth atom def palts

and genPAlts tenv env depth atom def =
    function
    | (lit, e) :: xs ->
        ([ genAtom tenv env depth atom
           [ lit
             Wasm.I32Eq
             Wasm.IfElse([ Wasm.I32 ], genExpr tenv env ( depth + 1u ) e, genPAlts tenv env ( depth + 1u ) atom def xs) ] ]
         |> List.concat)
    | [] -> genExpr tenv env depth def

and genAAlts tenv env depth atom def =
    function
    | ((c, vs), e) :: xs ->
        let constrIndex = c

        let assignLocals =
            vs
            |> List.mapi (fun i v ->
                let local = getLocal env (StgVar v)
                [ genAtom tenv env depth atom
                  [ Wasm.I32Load
                      { align = 0u
                        offset = 4u * (3u + uint32 (i)) } ]
                  [ Wasm.LocalSet local ] ]
                |> List.concat)

        // This must a data heap object - check Type Idx
        [ genAtom tenv env depth atom
          [ Wasm.I32Load
              { align = 0u
                offset = 8u }
            Wasm.I32Const(constrIndex)
            Wasm.I32Eq
            Wasm.IfElse
                ([ Wasm.I32 ],
                 [ assignLocals |> List.concat
                   genExpr tenv env ( depth + 1u ) e ]
                 |> List.concat, genAAlts tenv env ( depth + 1u ) atom def xs) ] ]
        |> List.concat
    | [] -> genExpr tenv env depth def

and genPrim tenv env depth atoms =
    atoms
    |> List.rev
    |> List.collect (genStgVarAtom tenv env depth)


let rec genLetFuncs = function
    | (b, lf) ->
        [ [ { name = StgVar b
              functype = stdFuncType
              indirect = true
              caf = false
              export = None
          } ]
          genLetsFuncs lf.lets ]
        |> List.concat

and genLetsFuncs = List.collect genLetFuncs


let genTopLamFuncs export b args (lf: LambdaForm) =
    [ [ { name = StgVar b
          functype = (Wasm.I32 |> List.replicate (args |> List.length), [ Wasm.I32 ])
          indirect = false
          caf = false
          export=export } ]
      genLetsFuncs lf.lets ]
    |> List.concat

let genTopCafFunc b lf =
    [
        [{
        name = StgVar b
        functype = stdFuncType
        indirect = true
        caf = true
        export = None
        }]
        genLetsFuncs lf.lets
    ] |> List.concat 

let genTopConstrFunc b vs =
    { name = StgVar b
      functype = (Wasm.I32 |> List.replicate (vs |> List.length), [ Wasm.I32 ])
      indirect = false
      caf = false
      export= None }

let genTopLevelFuncs =
    function
    | b:Stg.Var, TopLam (args, lam) -> genTopLamFuncs None b args lam
    | b:Stg.Var, TopExport (name, args, lam) -> genTopLamFuncs (Some name) b args lam
    | b, TopCaf lam -> genTopCafFunc b lam
    | b, TopConstr (_, vs) -> [ genTopConstrFunc b vs ]

let placeLocal local i = (local, Local i)
let placeLet (env:Map<Var, Placement>) ((b, lf: LambdaForm)) i =
    (StgVar b, Lambda(i, getIndirectFunc env (StgVar b), lf.args, lf.frees))
let stdConstrEnv env (b, f, vs) =
    (StgVar b), StdConstr(getFunc env (StgVar f), vs)

let localsEnv env args locals (lets:(Stg.Var*LambdaForm) list) =
    let wasmLocals =
        List.concat 
            [ args |> List.map (placeLocal)
              locals |> List.map (StgVar >> placeLocal)
              lets |> List.map (placeLet env) 
            ]
    Seq.initInfinite (uint32)
    |> Seq.zip wasmLocals
    |> Seq.map (fun (f, i) -> f i)

let rec genLetCode tenv env depth = function
    | (_, lf: LambdaForm) ->
        let newEnv =
            Seq.concat
                [ Map.toSeq env
                  localsEnv env [This] lf.locals lf.lets
                  lf.stdConstrs |> Seq.map (stdConstrEnv env )
                  Seq.initInfinite (uint32 >> Heap)
                  |> Seq.zip (ArgsRemaining :: ThisFunction :: List.concat [ lf.args |> List.rev |> List.map StgVar; lf.frees |> List.map StgVar]) ]
            |> Map.ofSeq

        [ [ Wasm.I32 |> List.replicate ((lf.locals |> List.length) + (lf.lets |> List.length)), genExpr tenv newEnv depth lf.expr ]
          genLetsCode tenv env depth lf.lets ]
        |> List.concat

and genLetsCode tenv env depth = List.collect (genLetCode tenv env depth)

let genTopBindCode export tenv env depth b args (lf: LambdaForm) =
    let resetHeap =
        match export with
        |true -> 
            let (Heap heapStart) = env |> Map.find HeapStart
            let cafs = env |> Map.toList |> List.map snd |> List.choose(function Caf(i, funcidx) -> Some(i+4u, funcidx)|_->None)
            let cafReset = cafs |> List.collect (fun (i, funcidx) -> [Wasm.I32Const (int32 i); Wasm.I32Const (int32 funcidx); Wasm.I32Store {align=0u; offset=0u}])
            [
                cafReset
                [Wasm.I32Const (int32 heapStart); Wasm.GlobalSet heapTop]
            ] |> List.concat
        |_ -> []
    if not (List.isEmpty lf.frees) then
        failwithf "Top level bindings cannot have free variables: %A" lf.frees
    else
    if not (List.isEmpty lf.args) then
        failwithf "Top level bindings cannot have args: %A" lf.frees
    else
        let newEnv =
            Seq.concat
                [ Map.toSeq env
                  lf.stdConstrs |> Seq.map (stdConstrEnv env )
                  localsEnv env (args |> List.map StgVar) lf.locals lf.lets 
                ]
            |> Map.ofSeq

        let wasmLocalTypes = Wasm.I32 |> List.replicate ((lf.locals |> List.length) + (lf.lets |> List.length))
        let expr = genExpr tenv newEnv depth lf.expr
        [ [wasmLocalTypes, List.concat [resetHeap; expr] ]
          genLetsCode tenv env depth lf.lets ]
        |> List.concat

let genTopConstrCode tenv env depth b c vs =
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
        Wasm.I32Const(c)
        Wasm.I32Store
            { align = 0u
              offset = 8u } ]
      vsStores |> List.concat
      [ Wasm.LocalGet localidx ] ]
    |> List.concat

let genTopLevelCode tenv env depth =
    function
    | b, TopLam (args, lam) -> genTopBindCode false tenv env depth b args lam
    | b, TopExport (_, args, lam) -> genTopBindCode true tenv env depth b args lam
    | b, TopCaf lam -> genTopBindCode false tenv env depth b [] lam
    | b, TopConstr(c, vs) -> [ genTopConstrCode tenv env depth b c vs ]

let rec genLetName tenv env depth = function
    | (b:Stg.Var, lf: LambdaForm) ->
        let args = ["this"]
        let locals = lf.locals |> List.map(fun x -> sprintf "%A" x)
        let lets = lf.lets|> List.map(fun x -> sprintf "%A" (fst x))
        let funcLocalNames = List.concat[args;locals;lets]

        let funcName = b.name
        let funcNames, localNames = genLetsNames tenv env depth lf.lets        
        ([
            [funcName]
            funcNames
        ] |> List.concat,
        [
            [funcLocalNames]
            localNames
        ] |> List.concat) : Names

and concatNames names = 
    names |> List.fold(fun (funcNames1, localNames1) (funcNames2, localNames2) ->    
        [
            funcNames1
            funcNames2
        ] |> List.concat,
        [
            localNames1
            localNames2
        ] |> List.concat
    ) ([], [])

and genLetsNames tenv env depth lets = 
    lets |> List.map (genLetName tenv env depth) |> concatNames
    

let genTopBindNames export tenv env depth (b:Stg.Var) args (lf: LambdaForm) : Names=
    if not (List.isEmpty lf.frees) then
        failwithf "Top level bindings cannot have free variables: %A" lf.frees
    else
    if not (List.isEmpty lf.args) then
        failwithf "Top level bindings cannot have args: %A" lf.frees
    else
        let funcName = b.name
        let funcLocalNames = List.concat [(args |> List.map (fun x -> sprintf "%A" x));(lf.locals |> List.map (fun x -> sprintf "%A" x)); (lf.lets |> List.map (fun x -> sprintf "%A" (fst x)))]
        let funcNames, localNames = genLetsNames tenv env depth lf.lets
        [
            [funcName]
            funcNames
        ] |> List.concat,
        [
            [funcLocalNames]
            localNames
        ] |> List.concat

let genTopConstrNames tenv env depth b c vs =   
    let funcName = (b:Stg.Var).name
    let funcLocalNames = vs |> List.map (fun (v:Stg.Var) -> sprintf "%A"  v)
    ([funcName], [funcLocalNames]) : Names
        
let genTopLevelNames tenv env depth =
    function
    | b, TopLam (args, lam) -> genTopBindNames false tenv env depth b args lam
    | b, TopExport (_, args, lam) -> genTopBindNames true tenv env depth b args lam
    | b, TopCaf lam -> genTopBindNames false tenv env depth b [] lam
    | b, TopConstr(c, vs) -> genTopConstrNames tenv env depth b c vs


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
    



let genProgram moduleName (program: Program) =
    let runtimeFuncs = [ identity; indirection; malloc; clone 2u; apply 0u; whnfEval 0u 1u ]

    let topLevelFuncs =
        List.concat
            [ runtimeFuncs
              |> List.map (fun x ->
                  { name = x.name
                    functype = x.functype
                    indirect = x.indirect
                    caf = false 
                    export = None
                    })
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
        topLevelFuncs
        |> List.indexed
        |> List.choose (function
            | (i, {export=Some name}) ->
                Some
                    { Wasm.Export.nm = name
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
        Seq.concat[
            Seq.initInfinite (fun i -> uint32 (4+i*16))
            |> Seq.zip (cafs)
            |> Seq.map(fun (x,i) -> (x.name, Caf(i, getIndirectFunc indirectFuncEnv x.name)))
        ]        
        |> Map.ofSeq
    let heapStart = 4+ (cafs |> List.length) * 16

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
              seq{HeapStart, Heap (uint32 heapStart)}
            ]
        |> Map.ofSeq

    let codeSec =
        [ runtimeFuncs |> List.map (fun f -> f.func)
          program |> List.collect (genTopLevelCode tenv env 0u) ]
        |> List.concat

    let funcNames, localNames = program |> List.map (genTopLevelNames tenv env 0u) |> concatNames
    let nameSec = {
        Wasm.NameData.moduleNameSubsec=moduleName
        Wasm.funcNameSubsec=
            List.concat [runtimeFuncs |> List.map (fun f -> f.debugName); funcNames]
            |> Seq.zip (Seq.initInfinite (fun i -> uint32((i)))) |> Seq.toList
       

            
        Wasm.localNameSubsec=
            List.concat [
                runtimeFuncs |> List.map(fun f -> f.debugLocalNames |> Seq.zip (Seq.initInfinite uint32) |> Seq.toList)
                localNames |> List.map(fun locals -> locals |> Seq.zip (Seq.initInfinite uint32:Wasm.LocalIdx seq) |> Seq.toList :Wasm.NameMap<_>)
            ]
            |> Seq.zip ((Seq.initInfinite (fun i -> uint32(i):Wasm.FuncIdx))) 
            |> Seq.toList   
            |> List.map(fun x -> (x:Wasm.IndirectNameAssoc<_,_>))
    }
        
       

    

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
      if dataInit.Length>0 then Wasm.DataSec [{data=0u; offset=[Wasm.I32Const 0]; init=dataInit}]
      Wasm.CustomSec(Wasm.NameSec nameSec)
    ]