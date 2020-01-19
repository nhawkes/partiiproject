module Transform

open Analysis
open Vars

let rec makeWrapper call vs = function
    |Core.Lam(v, e) -> makeWrapper call (v::vs) e
    |_ ->
        makeWrapperLambda call [] (vs |> List.rev)

and makeWrapperLambda call lams = function
    |v::vs -> Core.Lam(v, makeWrapperLambda call (v::lams) vs)
    |[] ->
        makeWrapperBind call [] (lams |> List.rev)

and makeWrapperBind call binds = function
    |v::vs -> 
        match v.analysis with
        |Lazy ->
            makeWrapperBind call (v::binds) vs
        |Strict _ ->
            let newUnique = (generateVar v.var.typ).unique
            let var = {var={v.var with unique=newUnique}; analysis=v.analysis} 
            Core.Case(Core.Var(v.var), var, ([], makeWrapperBind call (var::binds) vs))
        |HyperStrict ->
            let var = {var=generateVar v.var.typ; analysis=v.analysis} 
            Core.Case(Core.Var(v.var), var, ([], makeWrapperBind call (var::binds) vs))
    |[] ->
        makeWrapperCall call (binds)



and makeWrapperCall call : List<_> -> Core.Expr<Var, AnalysedVar<_>> = function
    |v::vs ->
        Core.App(makeWrapperCall call vs, Core.Var v.var)
    |[] -> Core.Var (call.var)


let rec wwTransform  = function
    |(b, Core.TopExpr (e:Core.Expr<Var, AnalysedVar<_>>))::xs -> 
        let workerVar = {b with var = {b.var with Vars.unique = Worker b.var.unique}}
        let wrapper = b, Core.TopExpr(makeWrapper workerVar [] e)
        let worker = workerVar, Core.TopExpr e
        wrapper::worker::wwTransform xs
    |(v, Core.TopConstr vs)::xs -> 
        (v, Core.TopConstr vs)::wwTransform xs
    |[] -> 
        []

let shouldInlineTopLevel b =
    match b.var.unique with
    |BuiltIn _ -> true
    |Worker (BuiltIn _) -> true
    |Worker (_) -> false
    |_ -> true

let rec getTopInlineMap = function
    |(v, Core.TopExpr e)::xs ->
        if shouldInlineTopLevel v then
            getTopInlineMap xs |> Map.add v.var e
        else
            getTopInlineMap xs
    |(v, Core.TopConstr vs)::xs -> getTopInlineMap xs
    |[] -> Map.empty




let rec renameBoundVarsInExpr env f : Core.Expr<Var, AnalysedVar<_>> -> Core.Expr<Var, AnalysedVar<_>> =
    function
    | Core.Var v -> 
        match env |> Map.tryFind v with
        |Some v2 -> Core.Var v2
        |None -> Core.Var v
    | Core.Lit l -> Core.Lit l
    | Core.Lam(b, e) -> 
        let newB = {b with var=f b.var}
        let newEnv = env |> Map.add b.var newB.var
        Core.Lam(newB, renameBoundVarsInExpr newEnv f e)
    | Core.Let(bs, e) -> 
        let newEnv, newBs = renameBoundVarsInBinds env f bs
        Core.Let(newBs, renameBoundVarsInExpr newEnv f e)
    | Core.Case(e, b, alts) -> 
        let newB = {b with var=f b.var}
        let newEnv = env |> Map.add b.var newB.var
        Core.Case(renameBoundVarsInExpr env f e, newB, renameBoundVarsInAlts newEnv f alts)
    | Core.App(a, b) -> Core.App(renameBoundVarsInExpr env f a, renameBoundVarsInExpr env f b)
    | Core.Prim ps -> Core.Prim(ps |> List.map (renameBoundVarsInPrim env f))
    | Core.Unreachable -> Core.Unreachable

and renameBoundVarsInBinds env f =
    function
    | Core.Rec bs ->                
        let bsMapping = bs |> List.map (fun (b, e) -> b, {b with var=f b.var}) |> Map.ofList
        let newEnv = 
            bs |> 
            List.fold(
                    fun env (b, _) -> 
                    env |> Map.add b.var (bsMapping |> Map.find b).var
            ) env
        let newBs = bs |> List.map (fun (b,e) -> (bsMapping |> Map.find b, renameBoundVarsInExpr env f e))
        newEnv, Core.Rec(newBs)
    | Core.NonRec(b, e) ->         
        let newB = {b with var=f b.var}
        let newEnv = env |> Map.add b.var newB.var
        newEnv, Core.NonRec(newB, renameBoundVarsInExpr env f e)
    | Core.Join(b, e) ->        
        let newB = {b with var=f b.var}
        let newEnv = env |> Map.add b.var newB.var
        newEnv, Core.Join(newB, renameBoundVarsInExpr env f e)


and renameBoundVarsInAlts env f = function
    | (cases, def) -> cases |> List.map (renameBoundVarsInAlt env f), renameBoundVarsInExpr env f def

and renameBoundVarsInAlt env f = function
    | (p, vs), e -> 
        let vsMapping = vs |> List.map (fun v -> v, {v with var=f v.var}) |> Map.ofList
        let newEnv = 
            vs |> 
            List.fold(
                    fun env (v) -> 
                    env |> Map.add v.var (vsMapping |> Map.find v).var
            ) env
        let newVs = vs |> List.map (fun v -> (vsMapping |> Map.find v))
        (renameBoundVarsInPat newEnv f p, newVs), renameBoundVarsInExpr newEnv f e

and renameBoundVarsInPat env f = function
    | Core.DataAlt v -> Core.DataAlt v
    | Core.LitAlt l -> Core.LitAlt l
    
and renameBoundVarsInPrim env f =
    function
    | Stg.AVar v ->         
        match env |> Map.tryFind v with
        |Some v2 -> Stg.AVar(v2)
        |None ->Stg.AVar(v)        
    | Stg.ALit l -> Stg.ALit l


let rec inlinerExpr (inlineMap:Map<Vars.Var, Core.Expr<Vars.Var, AnalysedVar<_>>>) = function    
    | Core.Var v -> 
        match inlineMap |> Map.tryFind v with
        |Some e -> 
            let inliner = inlineVar()
            let newE = e |> renameBoundVarsInExpr Map.empty inliner
            inlinerExpr inlineMap newE
        |None ->
            Core.Var v
    | Core.Lit l -> Core.Lit l
    | Core.Lam(v, e) -> Core.Lam(v, inlinerExpr inlineMap e)
    | Core.Let(bs, e) -> 
        Core.Let(inlinerBinds inlineMap bs, inlinerExpr inlineMap e)
    | Core.Case(e, b, alts) -> Core.Case(inlinerExpr inlineMap e, b, inlinerAlts inlineMap alts)
    | Core.App(a, b) -> 
        let newA = inlinerExpr inlineMap a
        let newB = inlinerExpr inlineMap b
        match newA with
        |Core.Lam(v, e) ->
            Core.Let(Core.NonRec(v, b), e) |> inlinerExpr inlineMap 
        |Core.Let(bs, e) ->
            Core.Let(bs, Core.App(e, b)) |> inlinerExpr inlineMap
        |_ ->
            Core.App(newA, newB)
    | Core.Prim ps -> Core.Prim(ps)
    | Core.Unreachable -> Core.Unreachable

and inlinerAlts inlineMap (alts, def) = 
    let newAlts = alts |> List.map(fun (p,e) -> (p, inlinerExpr inlineMap e))
    let newDef = inlinerExpr inlineMap def
    (newAlts, newDef)

and inlinerBinds inlineMap = function
    |Core.NonRec (v, e) -> Core.NonRec (v, inlinerExpr inlineMap e)
    |Core.Join (v, e) -> Core.Join (v, inlinerExpr inlineMap e)
    |Core.Rec bs -> Core.Rec (bs |> List.map (fun (v,e) -> v, inlinerExpr inlineMap e))

let rec inliner inlineMap = function
    |(b, Core.TopExpr e)::xs -> 
        (b, Core.TopExpr (inlinerExpr inlineMap e))::inliner inlineMap xs
    |(v, Core.TopConstr vs)::xs -> 
        (v, Core.TopConstr vs)::inliner inlineMap xs
    |[] -> 
        []


let transform program =     
    let analysis = program |> Analysis.analyse
    let ww = analysis |> wwTransform
    let topInlineMap = getTopInlineMap ww
    let result = inliner topInlineMap ww
    result
