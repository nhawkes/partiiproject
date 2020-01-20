module Transform

open Analysis
open Vars
let isBuiltIn b =
    match b.var.unique with
    |BuiltIn _ -> true
    |_ -> false
    
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



let rec getSpecializations args = function
    | Core.Var v -> Map.empty
    | Core.Lit l -> Map.empty
    | Core.Lam(v, e) -> getSpecializations args e
    | Core.Let(bs, e) -> 
        getSpecializations args e
    | Core.Case(Core.Var(v), _, (alts, def)) when args |> List.contains v ->
        Map.ofList[v, alts |> List.map(fun (p, e) -> p)]
    | Core.Case(_, b, ([_, e], Core.Unreachable)) -> 
        getSpecializations args e
    | Core.Case(_, b, (_, e)) -> 
        getSpecializations args e
    | Core.App(a, b) -> 
        getSpecializations args b
        
let rec wwTransform  = function
    |(b, Core.TopExpr e)::xs when isBuiltIn b ->
        (b, Core.TopExpr e)::wwTransform xs
    |(b, Core.TopExpr (e:Core.Expr<Var, AnalysedVar<_>>))::xs -> 
        let workerVar = {b with var = {b.var with Vars.unique = Worker b.var.unique}}
        let wrapper = b, Core.TopExpr(makeWrapper workerVar [] e)
        let worker = workerVar, Core.TopExpr e
        wrapper::worker::wwTransform xs
    |(v, Core.TopConstr vs)::xs -> 
        (v, Core.TopConstr vs)::wwTransform xs
    |[] -> 
        []






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
    | Core.AVar v ->         
        match env |> Map.tryFind v with
        |Some v2 -> Core.AVar(v2)
        |None ->Core.AVar(v)        
    | Core.ALit l -> Core.ALit l
    | Core.AWasm l -> Core.AWasm l

let rec isSmall = function
    |Core.Unreachable
    |Core.Var _ -> true
    |Core.Lam(_, Core.Unreachable) -> true
    |Core.Lit _ -> true
    |_ -> false

let rec simplifyExpr (inlineMap:Map<Vars.Var, Core.Expr<Vars.Var, AnalysedVar<_>>>) = function    
    | Core.Var v -> 
        match inlineMap |> Map.tryFind v with
        |Some e -> 
            let inliner = inlineVar()
            let newE = e |> renameBoundVarsInExpr Map.empty inliner
            newE
        |None ->
            Core.Var v
    | Core.Lit l -> Core.Lit l
    | Core.Lam(v, e) -> Core.Lam(v, simplifyExpr inlineMap e)
    | Core.Let(bs, e) -> 
        simplifyBinds inlineMap e bs 
    | Core.Case(e, b, alts) -> 
        simplifyCase inlineMap (e, b, alts)
    | Core.App(a, b) -> 
        let newA = simplifyExpr inlineMap a
        let newB = simplifyExpr inlineMap b
        match newA with
        |Core.Lam(v, e) -> 
            simplifyExpr (Map.ofList [v.var, newB]) e
        |Core.Lam(v, e) when false ->
            Core.Let(Core.NonRec(v, b), e) |> simplifyExpr inlineMap 
        |Core.Let(bs, e) when false ->
            Core.Let(bs, Core.App(e, b)) |> simplifyExpr inlineMap
        |Core.Case(e, v, ([], def)) when false ->
            Core.Case(e, v, ([], Core.App(def, b))) |> simplifyExpr inlineMap
        |_ ->
        Core.App(newA, newB) |> factorApp
    | Core.Prim ps -> 
        simplifyPrims inlineMap [] ps
    | Core.Unreachable -> Core.Unreachable

and factorApp = function
    |Core.App(f, Core.Case(e, b, ([p, pe], Core.Unreachable))) when f |> isSmall ->
        Core.Case(e, b, ([p, Core.App(f, pe) |> factorApp], Core.Unreachable))
    |Core.App(f, Core.Case(e, b, ([], def))) when f |> isSmall ->
        Core.Case(e, b, ([], Core.App(f, def) |> factorApp))
    |x -> x



and simplifyAlts inlineMap (alts, def) = 
    let newAlts = alts |> List.map(fun (p,e) -> (p, simplifyExpr inlineMap e))
    let newDef = simplifyExpr inlineMap def    
    (newAlts, newDef)



and simplifyBinds inlineMap e = function
    |Core.NonRec (v, rhs) ->
        let newRhs = simplifyExpr inlineMap rhs 
        // Inline small values
        if false && isSmall newRhs then
            printfn "Inlining: %A" newRhs
            let newsimplifyMap = inlineMap |> Map.add v.var newRhs
            simplifyExpr newsimplifyMap e
        else if false && v.analysis <> Lazy then
            printfn "Let-to-case: %A" newRhs            
            // Change strict expressions into a case
            Core.Case(newRhs, v, ([], simplifyExpr inlineMap e))
        else
            Core.Let(Core.NonRec (v, newRhs), (simplifyExpr inlineMap e))
    |Core.Join (b1, rhs) -> 
        match e, rhs with
        |Core.Case(e, b2, ([], Core.App(Core.Var(v1), Core.Var(v2)))), Core.Lam(b22, def) when b1.var=v1 && b2.var=v2 && b22.var=v2 ->
            Core.Case(simplifyExpr inlineMap e, b2, ([], simplifyExpr inlineMap def))
        |_ ->
        let newRhs = simplifyExpr inlineMap rhs 
        // Inline small values
        if isSmall newRhs then
            let newInlineMap = inlineMap |> Map.add b1.var newRhs
            simplifyExpr newInlineMap e
        else
            Core.Let(Core.Join (b1, newRhs), (simplifyExpr inlineMap e))
    |Core.Rec [] -> simplifyExpr inlineMap e
    |Core.Rec bs -> 
        Core.Let(Core.Rec (bs |> List.map (fun (v,rhs) -> v, simplifyExpr inlineMap rhs)), (simplifyExpr inlineMap e))

and simplifyCase inlineMap (e, b, (alts, def)) =
    let newE = simplifyExpr inlineMap e
    let (newAlts, newDef) = simplifyAlts inlineMap (alts, def)
    match newE, b, (newAlts, newDef) with
    |Core.App(Core.Var(v), value), b, (alts, def) when v.callType=Some ConstrCall ->        
        match alts |> List.tryPick(function ((Core.DataAlt v2, [bs1]), e) -> Some(bs1, e) |_ -> None) with
        |Some(bs1, altE) -> 
            if value |> isSmall then
                simplifyExpr (Map.ofList [bs1.var, value]) altE
            else
                Core.Case(value, bs1, ([], altE))
        |None ->  
            simplifyExpr inlineMap def
    |value, v, ([], _) when value |> isSmall ->
        match def with
        |Core.Prim _ ->
            // The case around prim is needed 
            Core.Case(value, v, (alts, newDef))
        |_ -> simplifyExpr (Map.ofList [v.var, value]) newDef
    |(e, b, (alts, def))  -> Core.Case(e, b, (alts, def))

and isConstr = function
    |Core.Var{callType=Some ConstrCall} -> true
    |_ -> false



and simplifyPrims inlineMap newPs = function
    |Core.AVar v::ps ->
        match inlineMap |> Map.tryFind v with
        |None -> simplifyPrims inlineMap (Core.AVar v::newPs) ps
        |Some(Core.Lit l) ->             
            simplifyPrims inlineMap (Core.ALit l::newPs) ps 
        |Some(x) -> 
            let inliner = inlineVar()
            let newV = {var=inliner v; analysis=Analysis.Strict 0}
            let inner = simplifyPrims inlineMap (Core.AVar newV.var::newPs) ps 
            Core.Case(x, newV, ([], inner))
    |Core.ALit l::ps -> 
        simplifyPrims inlineMap (Core.ALit l::newPs) ps
    |Core.AWasm w::ps -> 
        simplifyPrims inlineMap (Core.AWasm w::newPs) ps
    |[] -> Core.Prim (newPs |> List.rev)

    

let rec simplify inlineMap = function
    |(b, Core.TopExpr e)::xs -> 
        (b, Core.TopExpr (simplifyExpr inlineMap e))::simplify inlineMap xs
    |(v, Core.TopConstr vs)::xs -> 
        (v, Core.TopConstr vs)::simplify inlineMap xs
    |[] -> 
        []

let shouldInlineTopLevel b =
    match b.var.unique with
    |BuiltIn _ -> true
    |Worker (BuiltIn _) -> true
    |Worker (_) -> false
    |_ -> true



let rec getTopInlineMap shouldInline = function
    |(v, Core.TopExpr e)::xs ->
        if shouldInline v then
            getTopInlineMap shouldInline xs |> Map.add v.var e
        else
            getTopInlineMap shouldInline xs
    |(v, Core.TopConstr vs)::xs -> getTopInlineMap shouldInline xs
    |[] -> Map.empty




let transform program =     
    let analysis = program |> Analysis.analyse
    let ww = analysis |> wwTransform
    let topInlineMap = getTopInlineMap isBuiltIn analysis
    System.IO.File.WriteAllText("./Compiler/input.core", sprintf "%A" ww)
    let result = simplify topInlineMap ww
    System.IO.File.WriteAllText("./Compiler/output.core", sprintf "%A" result)
    result
