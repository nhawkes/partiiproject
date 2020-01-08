module WasmGen

open Stg

(*
Memory layout:
All
======
-4: Size
0: Whnf function
...: Data/Thunk/Function

Data
=======
4: Type Idx
...: Fields

Thunk
=======
...: frees

Func
=======
4: NextArgPosition
8: Function
...: args
...: frees



*)

type Var =
    | StgVar of Vars.Var
    | Identity
    | Malloc
    | Clone
    | Apply
    | This
    | ThisFunction
    | NextArgPosition

type Placement =
    | Heap of uint32
    | Local of Wasm.LocalIdx
    | Func of Wasm.FuncIdx
    | IndirectFunc of Wasm.TableIdx
    | Lambda of Wasm.LocalIdx * Wasm.FuncIdx * Args<Var> * Free<Var>
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
        match env |> Map.tryFind (v) with
        | Some(Heap i) ->
            [ Wasm.LocalGet 0u
              Wasm.I32Load
                  { align = 0u
                    offset = 4u * uint32 i } ]
        | Some(Local i) -> [ Wasm.LocalGet i ]
        | Some(Func i) -> [ Wasm.Call i ]
        | Some(Lambda(i, _, _, _)) -> [ Wasm.LocalGet i ]
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
    [ genBinds tenv env binds
      genExpr tenv env e ]
    |> List.concat

and genBinds tenv env = function
    | NonRec xs -> xs |> List.collect (genNonRec tenv env)
        

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
            Wasm.LocalGet localidx
            Wasm.I32Const(int32 funcidx)
            Wasm.I32Store
              { align = 0u
                offset = 4u * 0u } 
            Wasm.LocalGet localidx
            Wasm.I32Const(4 * (args |> List.length))
            Wasm.I32Store
              { align = 0u
                offset = 4u * 1u }
           
        ]
        storeFrees
    ] |> List.concat
and genApp tenv env v args =
    let whnf =
        [ genAtom tenv env (AVar (StgVar v))
          genAtom tenv env (AVar (StgVar v))
          [ Wasm.I32Load
              { align = 0u
                offset = 0u } ]
          [ Wasm.CallIndirect(tenv |> Map.find stdFuncType) ] ]
        |> List.concat

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
    |> List.collect (genStgVarAtom tenv env)


let rec genLetFuncs = function
    | (b, lf) ->
        [ [ { name = StgVar b
              functype = stdFuncType
              indirect = true } ]
          genLetsFuncs lf.lets ]
        |> List.concat

and genLetsFuncs = List.collect genLetFuncs

let genTopLamFuncs b (lf: LambdaForm<Vars.Var>) =
    [ [ { name = StgVar b
          functype = (Wasm.I32 |> List.replicate (lf.args |> List.length), [ Wasm.I32 ])
          indirect = false } ]
      genLetsFuncs lf.lets ]
    |> List.concat

let genTopConstrFunc b vs =
    { name = StgVar b
      functype = (Wasm.I32 |> List.replicate (vs |> List.length), [ Wasm.I32 ])
      indirect = false }

let genTopLevelFuncs =
    function
    | b:Vars.Var, TopLam lam -> genTopLamFuncs b lam
    | b, TopConstr vs -> [ genTopConstrFunc b vs ]

let placeLocal local i = (local, Local i)
let placeLet (env:Map<Var, Placement>) ((b, lf: LambdaForm<Vars.Var>)) i =
    (StgVar b, Lambda(i, getIndirectFunc env (StgVar b), lf.args |> List.map StgVar, lf.frees |> List.map StgVar))

let localsEnv env locals (lets:(Vars.Var*LambdaForm<Vars.Var>) list) =
    let wasmLocals =
        List.concat
            [ This :: (locals |> List.map StgVar) |> List.map placeLocal
              lets |> List.map (placeLet env) ]
    Seq.initInfinite (uint32)
    |> Seq.zip wasmLocals
    |> Seq.map (fun (f, i) -> f i)

let rec genLetCode tenv env = function
    | (_, lf: LambdaForm<Vars.Var>) ->
        let newEnv =
            Seq.concat
                [ Map.toSeq env
                  localsEnv env lf.locals lf.lets
                  Seq.initInfinite (uint32 >> Heap)
                  |> Seq.zip (ThisFunction :: NextArgPosition :: List.concat [ lf.args |> List.map StgVar; lf.frees |> List.map StgVar]) ]
            |> Map.ofSeq

        [ [ Wasm.I32 |> List.replicate ((lf.locals |> List.length) + (lf.lets |> List.length)), genExpr tenv newEnv lf.expr ]
          genLetsCode tenv env lf.lets ]
        |> List.concat

and genLetsCode tenv env = List.collect (genLetCode tenv env)


(*/ Memory layout:
-1: Size
0: Function
1: NextArgPosition
2..arity: Args
...: Free
*)
let genTopBindCode tenv env (lf: LambdaForm<_>) =
    if not (List.isEmpty lf.frees) then
        failwithf "Top level bindings cannot have free variables: %A" lf.frees
    else
        let newEnv =
            Seq.concat
                [ Map.toSeq env
                  localsEnv env lf.locals lf.lets
                  Seq.initInfinite (uint32 >> Local) |> Seq.zip (List.concat [ lf.args |> List.map StgVar; lf.locals |> List.map StgVar]) ]
            |> Map.ofSeq

        [ [ Wasm.I32 |> List.replicate ((lf.locals |> List.length) + (lf.lets |> List.length)), genExpr tenv newEnv lf.expr ]
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
        Wasm.I32Const(getFunc env (StgVar b) |> int)
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

(*
Memory layout:
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
            [ Wasm.Block([],
                [
                 Wasm.Loop ([],
                    [  
                       Wasm.MemorySize
                       Wasm.GlobalGet heapTop
                       Wasm.I32Const 16
                       Wasm.I32ShrU
                       Wasm.I32GtU
                       Wasm.BrIf 1u
                       Wasm.I32Const 1
                       Wasm.MemoryGrow
                       Wasm.Drop 
                       Wasm.Br 0u
                    ])
                ])
            ]
            

            // Load heapTop
            [ Wasm.LocalGet 0u
              Wasm.I32Store
                  { align = 0u
                    offset = 0u } ] 

            // Return (old) heapTop + 4          
          ]
          |> List.concat }

let clone mallocIdx =     
    let thisPointer = 0u
    let size = 1u
    let i = 2u
    let cloned = 3u
    { name = Clone
      functype = [ Wasm.I32 ], [ Wasm.I32 ]
      indirect = false
      func =
          [Wasm.I32; Wasm.I32; Wasm.I32; Wasm.I32],
          [ [
              Wasm.LocalGet thisPointer
              Wasm.I32Const -4
              Wasm.I32Add
              Wasm.I32Load
                { align = 0u
                  offset = 0u }
              Wasm.LocalSet size
              // Get size from This

              Wasm.LocalGet size
              Wasm.Call mallocIdx // Call Malloc
              Wasm.LocalSet cloned

              Wasm.I32Const 0
              Wasm.LocalSet i
              //i=0
            ]


            [ Wasm.Block([],
                    [Wasm.Loop ([],
                       [
                       // If(i>=size) break;
                       Wasm.LocalGet i
                       Wasm.LocalGet size
                       Wasm.I32GeU
                       Wasm.BrIf 1u

                        
                       // Destination
                       Wasm.LocalGet cloned
                       Wasm.LocalGet i
                       Wasm.I32Add

                       // Source
                       Wasm.LocalGet thisPointer
                       Wasm.LocalGet i
                       Wasm.I32Add

                       // Copy
                       Wasm.I32Load {align = 0u; offset = 0u}
                       Wasm.I32Store {align = 0u; offset = 0u}

                       // i+=4
                       Wasm.LocalGet i
                       Wasm.I32Const 4
                       Wasm.I32Add
                       Wasm.LocalSet i

                       // Continue
                       Wasm.Br 0u
                       ])
                    ])
            ]            
            [
                Wasm.LocalGet cloned
            ]

              
                    
          ]
          |> List.concat }

let apply stdFuncTypeIdx =     
    let thisPointer = 0u
    let arg = 1u
    let nextArgPosition = 2u
    { name = Apply
      functype = [ Wasm.I32; Wasm.I32 ], [ Wasm.I32 ]
      indirect = false
      func =
          [ Wasm.I32 ],
          [
              // Get nextArgPosition
              Wasm.LocalGet thisPointer
              Wasm.I32Load
                { align = 0u
                  offset = 4u }
              Wasm.LocalSet nextArgPosition                  

              Wasm.LocalGet nextArgPosition
              Wasm.LocalGet thisPointer
              Wasm.I32Add
              // Calculate position of next arg

              Wasm.LocalGet arg
              Wasm.I32Store { align = 0u; offset = 4u }
              // Store the next argument

              Wasm.LocalGet nextArgPosition
              Wasm.I32Const -4
              Wasm.I32Add
              Wasm.LocalSet nextArgPosition
              // Calculate next arg position

              Wasm.LocalGet nextArgPosition
              Wasm.I32Eqz
              
              Wasm.IfElse([Wasm.I32], 
                [                    
                    // Function now has enough argument to be called
                    Wasm.LocalGet thisPointer
                    Wasm.LocalGet thisPointer
                    Wasm.I32Load
                      { align = 0u
                        offset = 0u }
                    Wasm.CallIndirect(stdFuncTypeIdx)
                ],
                [
                    
                    // Update next arg pointer
                    Wasm.LocalGet thisPointer
                    
                    Wasm.LocalGet nextArgPosition
                    Wasm.I32Store
                      { align = 0u
                        offset = 4u }
                    Wasm.LocalGet thisPointer    
                ]              
              )
          ] }


let genProgram (program: Program<Vars.Var>) =
    let runtimeFuncs = [ identity; malloc; clone 1u; apply 0u ]

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
            | (i, StgVar {unique=Vars.Export nm}) ->
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
      Wasm.ExportSec({ nm = "Memory"; exportdesc = Wasm.ExportMem 0u}::exportSec)
      Wasm.ElemSec
          [ { table = 0u
              offset = [ Wasm.I32Const 0 ]
              init = indirectElems } ]
      Wasm.CodeSec(codeSec) ]
