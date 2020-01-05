module StgGen
 

let addFree v frees =
    match v with    
    | {Vars.unique=Vars.Global _ } -> frees
    | {unique=Vars.Local _ } -> v::frees

let genLit = function
    |Core.I32 i -> Wasm.I32Const i

let rec genExpr e =
    let lf = 
        match e with
        | Core.Var v -> genVar v
        | Core.Lit lit -> Stg.lambdaForm (Stg.Prim [Stg.ALit(genLit lit)])
        | Core.Lam(v, e) -> genLam [ v ] e
        | Core.Let(bs, e) -> genLet e bs
        | Core.Case(e, v, alts) -> genCase e v alts
        | Core.App(a, b) -> genApp [b] a
        | Core.Prim ps -> genPrim ps
    Stg.normLf lf
    

and genVar (v:Vars.Var) =
    let e = 
        match v.typ with
        |Ast.ValueT -> Stg.App(v, [])
        |Ast.FuncT _ -> Stg.App(v, [])
        |Ast.IntT -> Stg.Prim[Stg.AVar v]
        |_ -> failwith "TODO"

    let lf = Stg.lambdaForm e
    {lf with frees = lf.frees |> addFree v}

and genLam vs =
    function
    | Core.Lam(v, e) -> genLam (v :: vs) e
    | e ->
        let lf = genExpr e
        let frees = lf.frees |> List.filter(fun free -> not(vs |> List.contains free))
        { lf with args = vs |> List.rev; frees=frees }

and genLet e = function
    | Core.NonRec [] -> genExpr e
    | Core.NonRec [ v1, e1 ] ->
        let lf = genExpr e
        let lf1 = genExpr e1
        let innerFrees = 
            lf1.frees 
                |> List.filter (fun free -> not(lf.locals |> List.contains free))
                |> List.filter (fun free -> not(lf.args |> List.contains free))
                |> List.filter (fun free -> not(lf.lets |> List.map fst |> List.contains free))

        let lets =
            List.concat
                [ [ v1, lf1 ]
                  lf.lets ]
        let frees = lf.frees |> List.append innerFrees |> List.filter((<>) v1)
        { lf with
              frees=frees
              lets = lets
              expr = Stg.Let(Stg.NonRec [ v1 ], lf.expr) }

and genCase e v alts =
    let lf = genExpr e
    let stgAlts, lfAlts = genAlts alts
    let lfCombined = Stg.combineLf lf lfAlts
        
    let expr = Stg.Case(lf.expr, v, stgAlts)
    let locals = v :: lfCombined.locals
    let frees = lfCombined.frees |> List.filter((<>) v)
    { lfCombined with
          locals = locals
          frees = frees
          expr = expr }
          
and genAlts =
    function
    | ((Core.Var(v), vs), e) :: xs, def ->
        let lfDef = genExpr def
        genAAlt lfDef lfDef.expr [] v vs e xs
    | ((Core.Lit l, []), e) :: xs, def ->
        let lfDef = genExpr def
        genPAlt lfDef lfDef.expr [] l e xs
    | [], def ->
        let lfDef = genExpr def
        genPAlts lfDef lfDef.expr [] []

and genAAlt lfAcc def aalts v vs e xs =
    let lfE = genExpr e
    let lfCombined = Stg.combineLf lfE lfAcc
    let frees = lfCombined.frees |> List.filter(fun free -> not(vs |> List.contains free))
    let locals = lfCombined.locals |> List.append vs
    let lf = {lfCombined with frees=frees; locals=locals}
    genAAlts lf def
        (List.concat
            [ [ (v, vs), lf.expr ]
              aalts ]) xs

and genAAlts lfAcc def aalts =
    function
    | ((Core.Var(v), vs), e) :: xs -> genAAlt lfAcc def aalts v vs e xs
    | [] -> Stg.AAlts (aalts, def), lfAcc

and genPAlt lfAcc def palts l e xs =
    let lfE = genExpr e
    let lfCombined = Stg.combineLf lfE lfAcc
    genPAlts lfCombined def
        (List.concat
            [ [genLit l, lfCombined.expr ]
              palts ]) xs

and genPAlts lfAcc def (palts:Stg.PAlts<_>) =
    function
    | ((Core.Lit l, []), e) :: xs ->
        genPAlt lfAcc def palts l e xs
    | [] -> 
        Stg.PAlts (palts, def), lfAcc

and genApp args = function
    | Core.App(f, a) ->
        genApp (a::args) f
    | Core.Var v -> genPrimCall v [] args
        
and genConstr v vs = function
    |(Core.Var arg)::args ->
        let lf:Stg.LambdaForm<_> = genConstr v (Stg.AVar arg::vs) args
        let frees = lf.frees |> addFree arg
        {lf with frees=frees}
    |(Core.Prim [Stg.ALit arg])::args ->
        genConstr v (Stg.ALit arg::vs) args
    |arg::args ->
        let var = Vars.generateVar Ast.ValueT
        let lfInner = genConstr v (Stg.AVar var::vs) args
        let lfE = genExpr arg
        let lets = (var, lfE)::lfInner.lets
        let frees = List.concat [lfInner.frees; lfE.frees]
        {lfInner with lets=lets; frees=frees; expr=Stg.Let(Stg.NonRec [var], lfInner.expr)}


    |[] ->
        Stg.lambdaForm (Stg.Constr(v, vs))

and genPrimCall f xs = function        
    |(Core.Var arg)::args ->
        let lf = genPrimCall f (Stg.AVar arg::xs) args
        let frees = lf.frees |> addFree arg
        {lf with frees=frees}
        
    |(Core.Prim [Stg.ALit arg])::args ->
        genPrimCall f (Stg.ALit arg::xs) args        
    |(Core.Lit arg)::args ->
        genPrimCall f (Stg.ALit (genLit arg)::xs) args        
    |arg::args ->
        let var = Vars.generateVar Ast.ValueT
        let lfInner = genConstr f (Stg.AVar var::xs) args
        let lfE = genExpr arg
        let lets = (var, lfE)::lfInner.lets
        let frees = List.concat [lfInner.frees; lfE.frees]
        {lfInner with lets=lets; frees=frees; expr=Stg.Let(Stg.NonRec [var], lfInner.expr)}
    |[] ->
        Stg.lambdaForm (Stg.Prim(Stg.AVar f::xs))
and genPrim ps =
    let vars = ps |> List.choose(function |(Stg.AVar v) -> Some v |_ -> None)
    let lf = Stg.lambdaForm (Stg.Prim ps)
    let frees = vars |> List.fold (fun frees v -> frees |> addFree v) []
    {lf with frees=frees}


let genTopLevel =
    function
    | b, Core.TopExpr e ->
       b, Stg.TopLam (genExpr e)
    | b, Core.TopConstr vs ->
        b, Stg.TopConstr vs

let genProgram (core: Core.Program<Vars.Var>): Stg.Program<Vars.Var> =
    let program = core |> List.map genTopLevel
    program
