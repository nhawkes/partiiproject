module StgGen

type LambdaForm<'b> =
    { args: 'b list
      frees: 'b list
      locals: 'b list
      lets: ('b * LambdaForm<'b>) list
      expr: Stg.Expr<'b> }

let lambdaForm e =
    { args = []
      frees = []
      locals = []
      lets = []
      expr = e }

let combineLf lf1 lf2 = 
    { args = [ lf1.args; lf2.args ] |> List.concat
      frees = [ lf1.frees; lf2.frees ] |> List.concat
      locals = [ lf1.locals; lf2.locals ] |> List.concat
      lets = [ lf1.lets; lf2.lets ] |> List.concat
      expr = lf1.expr }      

let rec toStgLf { args=args;
      frees=frees;
      locals=locals;
      lets=lets;
      expr=expr;} =
      let letsStg = lets |> List.map (fun (b, lf) -> Stg.Lifted(b, toStgLf lf))
      ((args, frees, locals, letsStg), expr)

let rec genExpr =
    function
    | Core.Var v -> { lambdaForm (Stg.App(v, [])) with frees = [ v ] }
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
    let lfCombined = combineLf lf lfAlts
        
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
        let lfCombined = combineLf lf lfAcc
        genAAlts lfCombined def
            (List.concat
                [ [ (v, vs), lf.expr ]
                  aalts ]) xs
    | [] -> 
        let lf = genExpr def
        let lfCombined = combineLf lf lfAcc
        Stg.AAlts (aalts, lf.expr), lfCombined

and genPAlts lfAcc def palts =
    function
    | ((Core.Prim([Stg.ALit l]), []), e) :: xs ->
        let lf = genExpr e
        let lfCombined = combineLf lf lfAcc
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
       b, Stg.TopLam (genExpr e |> toStgLf)
    | b, Core.TopConstr vs ->
        b, Stg.TopConstr vs

let genProgram (core: Core.Program<'b>): Stg.Program<'b> =
    core |> List.map genTopLevel
