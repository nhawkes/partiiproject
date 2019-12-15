module StgGen
 
let rec genExpr =
    function
    | Core.Var v -> { Stg.lambdaForm (Stg.App(v, [])) with frees = [ v ] }
    | Core.Lit lit -> failwith "TODO"
    | Core.Lam(v, e) -> genLam [ v ] e
    | Core.Let(bs, e) -> genLet e bs
    | Core.Case(e, v, alts) -> genCase e v alts
    | Core.App(a, b) -> failwith "TODO"
    | Core.Prim ps -> failwith "TODO"

and genLam vs =
    function
    | Core.Lam(v, e) -> genLam (v :: vs) e
    | e ->
        let lf = genExpr e
        { lf with args = vs |> List.rev }

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
    { lfCombined with
          locals = locals
          expr = expr }

and genAlts =
    function
    | ((Core.Var(v), vs), e) :: xs, def ->
        let lf = genExpr e
        genAAlts lf def [ (v, vs), lf.expr ] xs
    | ((Core.Prim(p), []), e) :: xs, def ->
        failwith "TODO"

and genAAlts lfAcc def aalts =
    function
    | ((Core.Var(v), vs), e) :: xs ->
        let lf = genExpr e
        let lfCombined = Stg.combineLf lf lfAcc
        genAAlts lfCombined def
            (List.concat
                [ [ (v, vs), lf.expr ]
                  aalts ]) xs
    | [] -> 
        let lf = genExpr def
        let lfCombined = Stg.combineLf lf lfAcc
        Stg.AAlts (aalts, lf.expr), lfCombined

and genPAlts lfAcc def palts =
    function
    | ((Core.Prim([Stg.ALit l]), []), e) :: xs ->
        let lf = genExpr e
        let lfCombined = Stg.combineLf lf lfAcc
        genPAlts lfCombined def
            (List.concat
                [ [ l, lf.expr ]
                  palts ]) xs
    | [] -> Stg.PAlts (palts, def), lfAcc

and genApp args = function
    | Core.App(f, a) ->
        genApp (a::args) f
    | Core.Var v -> genConstr v [] args
        
and genConstr v vs = function
    |(Core.Var arg)::args ->
        genConstr v (Stg.AVar arg::vs) args
    |[] ->
        Stg.Constr(v, vs)


let genTopLevel =
    function
    | b, Core.TopExpr e ->
       b, Stg.TopLam (genExpr e)
    | b, Core.TopConstr vs ->
        b, Stg.TopConstr vs

let genProgram (core: Core.Program<'b>): Stg.Program<'b> =
    core |> List.map genTopLevel
