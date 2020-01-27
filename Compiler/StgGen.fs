module StgGen
open Vars

let genVar =
    let i = ref (-1)
    fun typ ->
        let next = !i
        i := !i - 1
        Gen typ, next

let rec forcedCallArityWithKind k = function 
    |Types.FuncT(k2, a, b) when k = k2 ->
        1 + forcedCallArityWithKind k b    
    |_ -> 0


type Env =
    |IsLam of int
    |IsCaf
    |IsJoinPoint of int
    |IsConstr of int
    |IsExport of string * int
    |IsPrim

let forcedCallArity env f =
    match env |> Map.tryFind f with
    |Some(IsLam x) -> Some x
    |Some(IsConstr x) -> Some x
    |_ -> None


let addFree env v frees =
        match env |> Map.tryFind v with
        | Some (IsLam _) -> frees
        | Some (IsConstr _) -> frees
        | Some (IsCaf) -> frees
        | _ -> v :: frees




let genLit env = function
    | Core.I32 i -> Wasm.I32Const i

let rec genExpr (env:Map<Var, Env>) e =
    let lf =
        match e with
        | Core.VarE (v:Vars.Var) -> genApp env [] (Core.varE v)
        | Core.Lit lit -> Stg.lambdaForm (Stg.Prim [ Stg.ALit(genLit env lit) ])
        | Core.LamE(v, e) -> genLam env [v] e
        | Core.LetE(l, bs, e) -> genLet env e (l, bs)
        | Core.CaseE(e, v, alts) -> genCase env e v alts
        | Core.App(a, b) -> genApp env [b] a
        | Core.Prim (w, es) -> genPrim env w es
        | Core.Unreachable -> genPrim env Wasm.Unreachable [ Core.Lit(Core.I32 -1) ]
    Stg.normLf lf


and genLam env vs =
    function
    | Core.LamE(v, e) -> genLam env (v :: vs) e
    | e ->
        let lf = genExpr env e
        let frees = lf.frees |> List.filter (fun free -> not (vs |> List.contains free))
        { lf with
              args = vs |> List.rev
              frees = frees }

and genLet env e =
    function
    | Core.NonRec, [v1, e1] -> genBindings env genNonRec [ v1, genExpr env e1 ] (genExpr env e)
    | Core.Join, [j] -> genLetJoin env j e
    | Core.Rec, vs -> genBindings env genRec (vs |> List.map(fun (v,e) -> v, genExpr env e)) (genExpr env e)
        
and genNonRec env (vs, expr) =
    match vs with
    |[] -> expr
    |v::vs -> Stg.Let(Stg.NonRec v, genNonRec env (vs, expr))
 
and genRec env (vs, expr) = 
    match vs with
    |[] -> expr
    |vs -> Stg.Let(Stg.Rec vs, expr)

        
and genBindings env mapExpr (lfEs:(Vars.Var*Stg.LambdaForm<_>) list) lf =
    let newStdConstrs, newLets = lfEs |> List.partition(fun (_, lfE) -> match lfE.expr with Stg.Constr(_)->true|_ -> false )
    let vs = newLets |> List.map fst

    let innerFrees =
        List.concat [
            newLets |> List.map snd |> List.collect(fun lf -> lf.frees) 
            newStdConstrs |> List.map snd |> List.collect(fun lf -> lf.frees) 
        ]            
        |> List.distinct
        |> List.filter (fun free -> not (lf.locals |> List.contains free))
        |> List.filter (fun free -> not (lf.args |> List.contains free))
        |> List.filter (fun free ->
            not
                (lf.lets
                 |> List.map fst
                 |> List.contains free))

    let stdConstrs = 
        List.concat [
            newStdConstrs |> List.map(function (v, {expr=Stg.Constr(f, vs)}) -> (v, f, vs))
            lf.stdConstrs
        ]

    let lets =
        List.concat
            [ newLets
              lf.lets ]

    let frees =
        lf.frees
        |> List.append innerFrees
        |> List.filter (fun v -> not(vs |> List.contains v))

    { lf with
          stdConstrs = stdConstrs
          frees = frees
          lets = lets
          expr = mapExpr env (vs, lf.expr) }

and genLetJoin env (j, eJ) e =
    let lf = genExpr (env |> Map.add j (IsJoinPoint 1) ) e
    let lfJ = genExpr env eJ
    let args = lfJ.args

    let innerFrees =
        lfJ.frees
        |> List.distinct
        |> List.filter (fun free -> not (lf.locals |> List.contains free))
        |> List.filter (fun free -> not (lf.args |> List.contains free))
        |> List.filter (fun free ->
            not
                (lf.lets
                 |> List.map fst
                 |> List.contains free))

    let frees =
        lf.frees
        |> List.append innerFrees
        |> List.filter ((<>)j)

    let expr =
        Stg.Let(
            Stg.Join(j, args, lfJ.expr),
            lf.expr
        )
    
    {
            lets = List.concat [lf.lets; lfJ.lets]
            locals = List.concat [args; lf.locals; lfJ.locals]
            args = []
            stdConstrs = []
            frees = frees
            expr = expr
    }

    





and genCase env e v alts =
    let lf = genExpr env e
    let def, otherAlts =
        match alts with
        |(Core.DefAlt, [], e)::otherAlts -> e, otherAlts
        |otherAlts -> Core.Unreachable, otherAlts
    let stgAlts, lfAlts = genAlts env def otherAlts
    let lfCombined = Stg.combineLf lf lfAlts

    let expr = Stg.Case(lf.expr, v, stgAlts)
    let locals = v :: lfCombined.locals
    let frees = lfCombined.frees |> List.filter ((<>) v)
    { lfCombined with
          locals = locals
          frees = frees
          expr = expr }

and genAlts env def =
    function
    | (Core.DataAlt v, vs, e) :: xs -> 
        let lfDef = genExpr env def
        genAAlt env lfDef lfDef.expr [] v vs e xs
    | (Core.LitAlt l, [], e) :: xs ->
        let lfDef = genExpr env def
        genPAlt env lfDef lfDef.expr [] l e xs
    | [] ->
        let lfDef = genExpr env def
        genPAlts env lfDef lfDef.expr [] []

and genAAlt env lfAcc def aalts v vs e xs =
    let lfE = genExpr env e
    let lfCombined = Stg.combineLf lfE lfAcc
    let frees = lfCombined.frees |> List.filter (fun free -> not (vs |> List.contains free))
    let locals = lfCombined.locals |> List.append vs

    let lf =
        { lfCombined with
              frees = frees
              locals = locals }
    genAAlts env lf def
        (List.concat
            [ [ (genConstr env v, vs), lf.expr ]
              aalts ]) xs

and genAAlts env lfAcc def aalts =
    function
    | (Core.DataAlt v, vs, e) :: xs -> genAAlt env lfAcc def aalts v vs e xs
    | [] -> Stg.AAlts(aalts, def), lfAcc

and genConstr env = function
| Core.IntDestr -> 1
| Core.Constr i -> 2 + i

and genPAlt env lfAcc def palts l e xs =
    let lfE = genExpr env e
    let lfCombined = Stg.combineLf lfE lfAcc
    genPAlts env lfCombined def
        (List.concat
            [ [ genLit env l, lfCombined.expr ]
              palts ]) xs

and genPAlts env lfAcc def (palts: Stg.PAlts<_>) =
    function
    | (Core.LitAlt l, [], e) :: xs -> genPAlt env lfAcc def palts l e xs
    | [] -> Stg.PAlts(palts, def), lfAcc


and genApp env args =
    function
    | Core.App(f, a) -> genApp env (a :: args) f
    | Core.VarE v -> genAppWithArgs env v args

and genAppWithArgs env f args =
    let arity = forcedCallArity env f
    match arity with 
    |None -> 
        genAtoms env (genAppWithAtoms env f) [] args
    |Some i when i = (args |> List.length) -> 
        // Saturated
        genAtoms env (fun xs -> Stg.lambdaForm (genCallWithAtoms env f xs)) [] args
    |Some i when i < (args |> List.length) -> 
        // Oversaturated
        let firstArgs = args |> List.take i
        let secondArgs = args |> List.skip i
        let firstResult = genAppWithArgs env f firstArgs
        genAtom env (fun f -> genAppWithArgs env f secondArgs) firstResult
    |Some i when i > (args |> List.length) -> 
        // Undersaturated
        let extraArgs = [(args |> List.length)..i] |> List.map (fun _ -> genVar Types.ValueT)
        let lf = genAppWithArgs env f (List.concat [args; extraArgs |> List.map Core.varE])
        let frees = lf.frees |> List.filter(fun v -> not(extraArgs |> List.contains(v)))
        {lf with args=List.concat[extraArgs; lf.args]; frees=frees}
    
and genAppWithAtoms env (f:Vars.Var) atoms =
    let e = genCallWithAtoms env f atoms
    let lf = Stg.lambdaForm e
    { lf with frees = lf.frees |> addFree env f }

and genCallWithAtoms env (f:Vars.Var) atoms =   
    match env |> Map.tryFind f with
    |Some (IsJoinPoint _) -> Stg.Jump(f, atoms)
    |Some (IsLam _) -> Stg.Call(f, atoms)
    |Some (IsConstr _) -> Stg.Constr(f, atoms)
    |_ -> Stg.App(f, atoms)


and genAtoms env mapAtoms xs =
    function
    | (Core.VarE arg) :: args ->
        let (lf:Stg.LambdaForm<_>) = genAtoms env mapAtoms (Stg.AVar arg :: xs) args
        let frees = lf.frees |> addFree env arg
        { lf with frees = frees }
    | (Core.Lit arg) :: args -> genAtoms env mapAtoms (Stg.ALit(genLit env arg) :: xs) args
    | arg :: args ->
        genAtom env (fun var -> genAtoms env mapAtoms (Stg.AVar var :: xs) args) (genExpr env arg)
    | [] -> mapAtoms (xs |> List.rev)

and genAtom env f (arg : Stg.LambdaForm<_>) =    
    let var = genVar Types.ValueT
    let lfInner = f var
    let lfE = arg
    genBindings env (genNonRec) [var, lfE] lfInner

and genPrim env w ps =
    genAtoms env (fun atoms -> Stg.lambdaForm (Stg.Prim(Stg.ALit w::atoms))) [] ps

let rec genTopLam env x args = function
    |Core.LamE(a, b) -> genTopLam env (x-1) (a::args) b
    |e when x = 0 -> 
        let lf = genExpr env e
        let frees = lf.frees |> List.filter(fun x -> not(args |> List.contains x))
        args|>List.rev, {lf with frees=frees} 

let genTopLevel env =
    function
    | b, Core.TopExpr (_, e) -> 
        match env |> Map.tryFind b with
        |Some IsCaf -> b, Stg.TopCaf(genExpr env e)
        |Some (IsExport (name, arity)) -> 
            let args, e = genTopLam env arity [] e
            b, Stg.TopExport(name, args, e)
        |Some (IsLam arity) -> b, Stg.TopLam(genTopLam env arity [] e)
    | b, Core.TopConstr(c, vs) -> b, Stg.TopConstr (genConstr c, vs)

let rec getManifestArity = function
    |Core.LamE(a, b) -> 1 + getManifestArity b
    |_ -> 0

let genProgram (Core.Program core): Stg.Program<Vars.Var> =
    let env = core |> List.map(function
        | b, Core.TopConstr (c, vs) -> b, IsConstr (vs.Length)
        | b, Core.TopExpr (Core.NoExport, e) when getManifestArity e > 0 -> b, IsLam (getManifestArity e)
        | b, Core.TopExpr (Core.NoExport, e) -> b, IsCaf
        | b, Core.TopExpr (Core.Export s, e) -> b, IsExport (s, getManifestArity e)
    ) 
    let program = core |> List.map (genTopLevel (env |> Map.ofList))
    program
