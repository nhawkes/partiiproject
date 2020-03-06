module StgGen
open Vars


let genVar =
    let i = ref (-1)
    fun () ->
        let next = !i
        i := !i - 1
        {Stg.name="gen"+string next; Stg.info=None; Stg.unique=next; Stg.prim=false}
let i = ref (1)
let wrapVar s : Analysis.AnalysedVar<Vars.Var> -> Stg.Var =
    fun (v) ->
        let next = !i
        i := !i + 1
        {Stg.name=s; Stg.info=Some (v:>obj); Stg.unique=next; Stg.prim=v.var.typ=Types.IntT}

let wrapBinder (((s, i), x):Core.Binder<_>) = (s, i), wrapVar (sprintf "%s_%i" s i) x


type TopEnv =
    |IsLam of int
    |IsCaf
    |IsJoinPoint of int
    |IsConstr of int
    |IsExport of string * int
    |IsPrim
    |IsValue

let forcedCallArity topEnv f =
    match topEnv |> Map.tryFind f with
    |Some(IsLam x) -> Some x
    |Some(IsConstr x) -> Some x
    |_ -> None


let addFree topEnv  v frees =
        match topEnv |> Map.tryFind v with
        | Some (IsLam _) -> frees
        | Some (IsConstr _) -> frees
        | Some (IsCaf) -> frees
        | _ -> v :: frees


let rec withBinders env = function
    |(v, k)::xs -> withBinders (env |> Map.add k v) xs
    |[] -> env
    
let rec withPrimBinders env  =
    function
    |(_, k)::xs -> withPrimBinders (env |> Map.add k IsPrim) xs
    |[] -> env
    

let genLit topEnv = function
    | Core.I32 i -> Wasm.I32Const i

let rec genExpr (topEnv:Map<Stg.Var, TopEnv>) e =
    let lf =
        match e:Core.Expr<Stg.Var> with
        | Core.VarE (v) -> genApp topEnv [] (Core.varE v)
        | Core.Lit lit -> Stg.lambdaForm (Stg.Prim [ Stg.ALit(genLit topEnv lit) ])
        | Core.LamE(v:Core.Binder<Stg.Var>, e) -> genLam topEnv [snd v] e
        | Core.LetE(l, bs, e) -> genLet topEnv e (l, bs)
        | Core.CaseE(e, v, alts) -> genCase topEnv  e (v) alts
        | Core.App(a, b) -> genApp topEnv [b] a
        | Core.Prim (w, es) -> genPrim topEnv w es
        | Core.Unreachable -> genPrim topEnv Wasm.Unreachable [ Core.Lit(Core.I32 -1) ]
    Stg.normLf lf


and genLam topEnv vs =
    function
    | Core.LamE(v, e) -> genLam topEnv (snd v :: vs) e
    | e ->
        let lf = genExpr topEnv e
        let frees = lf.frees |> List.filter (fun free -> not (vs |> List.contains free))
        { lf with
              args = vs |> List.rev
              frees = frees }

and genLet topEnv e =
    function
    | Core.NonRec, [v1, e1] -> genBindings topEnv genNonRec [ v1 |>snd, genExpr topEnv e1 ] (genExpr topEnv e)
    | Core.Join, [j] -> genLetJoin topEnv j e
    | Core.Rec, vs -> 
        genBindings topEnv genRec (vs |> List.map(fun (v,e) -> snd v, genExpr topEnv e)) (genExpr topEnv e)
        
and genNonRec topEnv (vs, expr) =
    match vs with
    |[] -> expr
    |v::vs -> Stg.Let(Stg.NonRec v, genNonRec topEnv (vs, expr))
 
and genRec topEnv (vs, expr) = 
    match vs with
    |[] -> expr
    |vs -> Stg.Let(Stg.Rec vs, expr)

        
and genBindings topEnv mapExpr (lfEs:(_*Stg.LambdaForm) list) lf =
    let newStdConstrs, newLets = lfEs |> List.partition(fun (_, lfE) -> match lfE.args, lfE.expr with [], Stg.Constr(_)->true|_ -> false )
    let vs = List.concat[newLets |> List.map fst]

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
            newStdConstrs |> List.collect(function 
                (v, ({args=[]; expr=Stg.Constr(f, vs)} as lfStdConstrs)) -> 
                    (v, f, vs)::lfStdConstrs.stdConstrs)
            lf.stdConstrs
        ]

    let lets =
        List.concat
            [ newLets |> List.map(fun (x,y) -> x, y)
              lf.lets ]

    let frees =
        lf.frees
        |> List.append innerFrees
        |> List.filter (fun v -> not(newStdConstrs |> List.map fst |> List.contains v))
        |> List.filter (fun v -> not(newLets |> List.map fst |> List.contains v))


    { lf with
          stdConstrs = stdConstrs
          frees = frees
          lets = lets
          expr = mapExpr topEnv (vs, lf.expr) }

and genLetJoin topEnv (j, eJ) e =
    let lf = genExpr (topEnv |> Map.add (snd j) (IsJoinPoint 1)) e
    let lfJ = genExpr topEnv eJ
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
        |> List.filter ((<>)(snd j))

    let expr =
        Stg.Let(
            Stg.Join(snd j, args, lfJ.expr),
            lf.expr
        )
    
    {
            lets = List.concat [lf.lets; lfJ.lets]
            locals = List.concat [args; lf.locals; lfJ.locals]
            args = []
            stdConstrs = List.concat [lf.stdConstrs; lfJ.stdConstrs]
            frees = frees
            expr = expr
    }

    





and genCase topEnv e v alts =
    let lf = genExpr topEnv e
    let def, otherAlts =
        match alts with
        |(Core.DefAlt, [], e)::otherAlts -> e, otherAlts
        |otherAlts -> Core.Unreachable, otherAlts
    let stgAlts, lfAlts = genAlts (withPrimBinders topEnv [v]) def otherAlts
    let lfCombined = Stg.combineLf lf lfAlts

    let expr = Stg.Case(lf.expr, snd v, stgAlts)
    let locals = snd v :: lfCombined.locals
    let frees = lfCombined.frees |> List.filter ((<>) (snd v))
    { lfCombined with
          locals = locals
          frees = frees
          expr = expr }

and genAlts topEnv def =
    function
    | (Core.DataAlt v, vs, e) :: xs -> 
        let lfDef = genExpr topEnv def
        let newTopEnv = 
            if v = Core.IntDestr then
                withPrimBinders topEnv vs
            else
                topEnv
        genAAlt newTopEnv lfDef lfDef.expr [] v (vs |> List.map snd) e xs
    | (Core.LitAlt l, [], e) :: xs ->
        let lfDef = genExpr topEnv def
        genPAlt topEnv lfDef lfDef.expr [] l e xs
    | [] ->
        let lfDef = genExpr topEnv def
        genPAlts topEnv lfDef lfDef.expr [] []

and genAAlt topEnv lfAcc def aalts v vs e xs =
    let lfE = genExpr topEnv e
    let lfCombined = Stg.combineLf lfE lfAcc
    let frees = lfCombined.frees |> List.filter (fun free -> not (vs |> List.contains free))
    let locals = lfCombined.locals |> List.append vs

    let lf =
        { lfCombined with
              frees = frees
              locals = locals }
    genAAlts topEnv lf def
        (List.concat
            [ [ (genConstr topEnv v, vs), lf.expr ]
              aalts ]) xs

and genAAlts topEnv lfAcc def aalts =
    function
    | (Core.DataAlt v, vs, e) :: xs -> genAAlt topEnv lfAcc def aalts v (vs |> List.map snd) e xs
    | [] -> Stg.AAlts(aalts, def), lfAcc

and genConstr topEnv = function
| Core.IntDestr -> 1
| Core.BoolDestr -> 2
| Core.Constr i -> 3 + i

and genPAlt topEnv lfAcc def palts l e xs =
    let lfE = genExpr topEnv e
    let lfCombined = Stg.combineLf lfE lfAcc
    genPAlts topEnv lfCombined def
        (List.concat
            [ [ genLit topEnv l, lfCombined.expr ]
              palts ]) xs

and genPAlts topEnv lfAcc def (palts: Stg.PAlts) =
    function
    | (Core.LitAlt l, [], e) :: xs -> genPAlt topEnv lfAcc def palts l e xs
    | [] -> Stg.PAlts(palts, def), lfAcc


and genApp topEnv args =
    function
    | Core.App(f, a) -> genApp topEnv (a :: args) f
    | Core.VarE v -> genAppWithArgs topEnv (snd v) args

and genAppWithArgs topEnv (f:Stg.Var) args =
    let arity = forcedCallArity topEnv (f)
    match arity with 
    |None -> 
        genAtoms topEnv (genAppWithAtoms topEnv (f)) [] args
    |Some i when i = (args |> List.length) -> 
        // Saturated
        genAtoms topEnv (fun xs -> Stg.lambdaForm (genCallWithAtoms topEnv (f) xs)) [] args
    |Some i when i < (args |> List.length) -> 
        // Oversaturated
        let firstArgs = args |> List.take i
        let secondArgs = args |> List.skip i
        let firstResult = genAppWithArgs topEnv f firstArgs
        genAtom topEnv (fun f -> genAppWithArgs topEnv (f) secondArgs) firstResult
    |Some i when i > (args |> List.length) -> 
        let etaArgs = [1..i] |> List.map(fun _ -> genVar())
        let etaExpanded = genCallWithAtoms topEnv (f) (etaArgs |> List.map Stg.AVar)
        let lfEta = {Stg.lambdaForm etaExpanded with args=etaArgs}
        let var = genVar()
        genBindings topEnv (genNonRec) [var, lfEta] (genAtoms topEnv (genAppWithAtoms topEnv var) [] args)
     
and genAppWithAtoms topEnv (f:Stg.Var) atoms =
    let e = genCallWithAtoms topEnv f atoms
    let lf = Stg.lambdaForm e
    { lf with frees = lf.frees |> addFree topEnv f }

and genCallWithAtoms topEnv (f:Stg.Var) atoms =   
    match f with
    |{prim=true} -> Stg.Prim(Stg.AVar f::atoms)
    |_ ->
    match topEnv |> Map.tryFind f with
    |Some (IsJoinPoint _) -> Stg.Jump(f, atoms)
    |Some (IsLam _) -> Stg.Call(f, atoms)
    |Some (IsConstr _) -> Stg.Constr(f, atoms)
    |Some (IsPrim _) -> Stg.Prim(Stg.AVar f::atoms)
    |_ -> Stg.App(f, atoms)


and genAtoms topEnv mapAtoms xs =
    function
    | (Core.VarE arg) :: args when forcedCallArity topEnv (snd arg) = None->
        let (lf:Stg.LambdaForm) = genAtoms topEnv mapAtoms (Stg.AVar (snd arg) :: xs) args
        let frees = lf.frees |> addFree topEnv (snd arg)
        { lf with frees = frees }
    | (Core.Lit arg) :: args -> genAtoms topEnv mapAtoms (Stg.ALit(genLit topEnv arg) :: xs) args
    | arg :: args ->
        genAtom topEnv (fun var -> genAtoms topEnv mapAtoms (Stg.AVar var :: xs) args) (genExpr topEnv arg)
    | [] -> mapAtoms (xs |> List.rev)

and genAtom topEnv (f:Stg.Var -> Stg.LambdaForm) (arg : Stg.LambdaForm) =    
    let var = genVar()
    let lfInner = f var
    let lfE = arg
    genBindings topEnv (genNonRec) [var, lfE] lfInner

and genPrim topEnv w ps =
    genAtoms topEnv (fun atoms -> Stg.lambdaForm (Stg.Prim(Stg.ALit w::atoms))) [] ps

let rec genTopLam topEnv x args = function
    |Core.LamE(a, b) -> genTopLam topEnv (x-1) (snd a::args) b
    |e when x = 0 -> 
        let lf = genExpr topEnv e
        let frees = lf.frees |> List.filter(fun x -> not(args |> List.contains (x)))
        if frees.Length <> 0 then failwith "Cannot have frees"
        args|>List.rev, {lf with frees=frees} 

let genTopLevelExpr topEnv =
    function
    | b, (_, e) -> 
        match topEnv |> Map.tryFind (snd b) with
        |Some IsCaf -> snd b, Stg.TopCaf(genExpr topEnv e)
        |Some (IsExport (name, arity)) -> 
            let args, lf = genTopLam topEnv arity [] e
            snd b, Stg.TopExport(name, args, lf)
        |Some (IsLam arity) -> snd b, Stg.TopLam(genTopLam topEnv arity [] e)
let genTopLevelConstr topEnv =
    function
    | b, (c, vs) -> snd b, Stg.TopConstr (genConstr topEnv c, vs |> List.map snd)

let rec getManifestArity = function
    |Core.LamE(a, b) -> 1 + getManifestArity b
    |_ -> 0




let rec uniqueifyExpr : Core.Expr<'a> -> Core.Expr<Stg.Var> = function
    | Core.VarS(v) -> Core.varS (v)
    | Core.Lit(l) -> Core.Lit(l)
    | Core.LamE(x, e) -> Core.lamE(wrapBinder x, uniqueifyExpr e) 
    | Core.LetE(l, bs, e) ->  Core.letE(l, uniqueifyBinds bs, uniqueifyExpr e)
    | Core.CaseE(e, b, alts) ->  Core.caseE(uniqueifyExpr e, wrapBinder b, uniqueifyAlts alts)
    | Core.App(a, b) -> Core.App(uniqueifyExpr a, uniqueifyExpr b)
    | Core.Prim(p, es) -> Core.Prim(p, es |> List.map (uniqueifyExpr))
    | Core.Unreachable -> Core.Unreachable

and uniqueifyAlts alts =
    alts |> List.map(fun (a,bs,e)->a, bs |> List.map wrapBinder, uniqueifyExpr e)

and uniqueifyBinds = function
    |(b, e)::xs -> (wrapBinder b, uniqueifyExpr e)::uniqueifyBinds xs
    |[] -> []
    
and uniquifyProgram (program:Core.Program<_>) : Core.ClosedProgram<Stg.Var> =
    let uniqueConstrs =
        program.constrs |> List.map(function
            | b, (c, vs) -> wrapBinder b, (c, vs |> List.map (fun ((s, x)) -> (s, wrapVar s x)))
        ) 
    let uniqueExprs = 
        program.exprs |> List.map(function
            | b, (export, e) -> wrapBinder b, (export, uniqueifyExpr e)
        ) 
    Core.closeProgram {constrs=uniqueConstrs; exprs=uniqueExprs}

let genProgram (Core.Program core): Stg.Program =
    let (Core.Program unique) = uniquifyProgram core
    
    let topEnv =
        List.concat[
            unique.constrs |> List.map(function
                | b, (c, vs) -> snd b, IsConstr (vs.Length)
            ) 
            unique.exprs |> List.map(function
                | b, (Core.NoExport, e) when getManifestArity e > 0 -> snd b, IsLam (getManifestArity e)
                | b, (Core.NoExport, e) -> snd b, IsCaf
                | b, (Core.Export s, e) -> snd b, IsExport (s, getManifestArity e)
            ) 

        ]
    let program =
        List.concat[
            unique.constrs |> List.map (genTopLevelConstr (topEnv |> Map.ofList))
            unique.exprs |> List.map (genTopLevelExpr (topEnv |> Map.ofList))
        ] 
    program
