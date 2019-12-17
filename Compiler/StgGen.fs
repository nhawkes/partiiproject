module StgGen
 
let rec genExpr =
    function
    | Core.Var v -> { Stg.lambdaForm (Stg.App(v, [])) with frees = [ v ] }
    | Core.Lit lit -> failwith "TODO"
    | Core.Lam(v, e) -> genLam [ v ] e
    | Core.Let(bs, e) -> genLet e bs
    | Core.Case(e, v, alts) -> genCase e v alts
    | Core.App(a, b) -> genApp [b] a
    | Core.Prim ps -> genPrim ps

and genLam vs =
    function
    | Core.Lam(v, e) -> genLam (v :: vs) e
    | e ->
        let lf = genExpr e
        let frees = lf.frees |> List.filter(fun free -> not(vs |> List.contains free))
        { lf with args = vs |> List.rev; frees=frees }

and genLet e = function
    | Core.NonRec [ v1, e1 ] ->
        let lf = genExpr e

        let lets =
            List.concat
                [ [ v1, genExpr e1 ]
                  lf.lets ]
        { lf with
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
    | ((Core.Prim([Stg.ALit l]), []), e) :: xs, def ->
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
            [ [ l, lfCombined.expr ]
              palts ]) xs

and genPAlts lfAcc def palts =
    function
    | ((Core.Prim([Stg.ALit l]), []), e) :: xs ->
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
        let frees = arg::lf.frees
        {lf with frees=frees}
    |(Core.Prim [Stg.ALit arg])::args ->
        genConstr v (Stg.ALit arg::vs) args
    |[] ->
        Stg.lambdaForm (Stg.Constr(v, vs))

and genPrimCall f xs = function        
    |(Core.Var arg)::args ->
        let lf = genConstr f (Stg.AVar arg::xs) args
        let frees = arg::lf.frees
        {lf with frees=frees}
    |(Core.Prim [Stg.ALit arg])::args ->
        genConstr f (Stg.ALit arg::xs) args
    |[] ->
        Stg.lambdaForm (Stg.Prim(Stg.AVar f::xs))

and genPrim ps =
    Stg.lambdaForm (Stg.Prim ps)


let genTopLevel =
    function
    | b, Core.TopExpr e ->
       b, Stg.TopLam (genExpr e)
    | b, Core.TopConstr vs ->
        b, Stg.TopConstr vs

let genProgram (core: Core.Program<'b>): Stg.Program<'b> =
    let program = core |> List.map genTopLevel
    program
