module Transform

open Analysis
open Vars
let isBuiltIn b =
    match b.var.unique with
    |BuiltIn _ -> true
    |_ -> false
    
let rec getArgSpecializations args = function
    | Core.Var v -> Map.empty
    | Core.Lit l -> Map.empty
    | Core.Lam(v, e) -> getArgSpecializations args e
    | Core.Let(bs, e) -> 
        getArgSpecializations args e
    | Core.Case(Core.Var(v), _, (alts, def)) when args |> List.contains v ->
        Map.ofList[v, alts |> List.map(fun (p, e) -> p)]
    | Core.Case(_, b, ([_, e], Core.Unreachable)) -> 
        getArgSpecializations args e
    | Core.Case(_, b, (_, e)) -> 
        getArgSpecializations args e
    | Core.App(a, b) -> 
        getArgSpecializations args b    

type ResultSpec =
    |TopResultSpec
    |ResultSpec of Vars.Var
    |BotResultSpec
    
let rec getResultSpecialization recCall = function
    | Core.Var v -> TopResultSpec
    | Core.Lit l -> TopResultSpec
    | Core.Lam(v, e) -> getResultSpecialization recCall e
    | Core.Let(bs, e) -> 
        getResultSpecialization recCall e
    | Core.Case(_, b, (alts, def)) -> 
        let altsE = alts |> List.map snd
        let result = 
            def::altsE |> 
            List.map (getResultSpecialization recCall) |> 
            List.fold (fun x y ->
                match x,y with
                |TopResultSpec, _ -> TopResultSpec
                |_, TopResultSpec -> TopResultSpec
                |ResultSpec v, ResultSpec v2 when v=v2 -> ResultSpec v
                |BotResultSpec, x -> x
                |x, BotResultSpec -> x
                |_ -> TopResultSpec
            ) BotResultSpec
        result        
    | Core.App(Core.Var v, b) -> 
        if v.unique = recCall.var.unique || Worker(v.unique) = recCall.var.unique then
            BotResultSpec
        else if v.callType = Some ConstrCall then
            ResultSpec v
        else
            TopResultSpec
    | Core.Unreachable -> BotResultSpec


let rec makeWrapper call vs = function
    |Core.Lam(v, e) -> makeWrapper call (v::vs) e
    |e ->
        let toSpecialize = getArgSpecializations (vs |> List.map (fun v -> v.var)) e
        let specializations, extraProgram =
            match vs with
            |[] -> Map.empty, []
            |v::_ ->
                match toSpecialize |> Map.tryFind v.var with
                |Some([Core.DataAlt(c), [value]]) ->
                    let specCall = {var=specializeVar() call.var; analysis=call.analysis} 
                    let specResult = getResultSpecialization call e
                    Map.ofList [v, [c, value, specCall.var, specResult]], [(specCall, Core.TopExpr (makeSpecialization specResult e [v,c, value]))]
                |None -> Map.empty, []
        makeWrapperLambda call specializations [] (vs |> List.rev), extraProgram

and makeSpecialization specResult e = function
    |[v, c, value] ->
        let newUnique = (generateVar value.var.typ).unique
        let lam = {var={value.var with unique=newUnique}; analysis=value.analysis}          
        match specResult with
        |ResultSpec res ->
            let bindVar = {var=(generateVar value.var.typ); analysis=Analysis.Strict 0}
            let resultVar = {var=(generateVar Types.IntT); analysis=Analysis.Strict 0}
            Core.Lam(lam, 
                Core.Case(Core.App(Core.Var(c), Core.Var(lam.var)), v, ([], 
                Core.Case(e, bindVar,
                ([(Core.DataAlt res, [resultVar]),  Core.Var(resultVar.var)],
                    Core.Unreachable)))                
            ))
        |_ -> Core.Lam(lam, Core.Case(Core.App(Core.Var(c), Core.Var(lam.var)), v, ([], e)))


and makeWrapperLambda call specializations lams = function
    |v::vs -> Core.Lam(v, makeWrapperLambda call specializations (v::lams) vs)
    |[] ->
        makeWrapperBind call specializations [] (lams |> List.rev)

and makeWrapperBind call specializations binds = function
    |v::vs -> 
        match v.analysis with
        |Lazy ->
            makeWrapperBind call specializations (v::binds) vs
        |Strict _ ->
            let newUnique = (generateVar v.var.typ).unique
            let var = {var={v.var with unique=newUnique}; analysis=v.analysis} 
            let alts = specializations |> Map.tryFind v |> Option.defaultValue [] |> specializationAlts
            Core.Case(Core.Var(v.var), var, (alts, makeWrapperBind call specializations (var::binds) vs))
        |HyperStrict ->
            let newUnique = (generateVar v.var.typ).unique
            let var = {var={v.var with unique=newUnique}; analysis=v.analysis} 
            Core.Case(Core.Var(v.var), var, ([], makeWrapperBind call specializations (var::binds) vs))
    |[] ->
        makeWrapperCall call (binds)

and specializationAlts = function
    |(c, value, specCall, specResult)::specs ->
        let alts = specializationAlts specs
        let v = {var=specializeVar() value.var; analysis=value.analysis} 
        let eval = {var=specializeVar() value.var; analysis=value.analysis} 
        match specResult with
        |ResultSpec res  ->
            ((Core.DataAlt(c), [v]), 
                Core.Case(Core.App(Core.Var(specCall), Core.Var(v.var)), eval, ([], 
                    Core.App(Core.Var res, Core.Var(eval.var))
                ))
            
            )::alts
        |_ ->
            ((Core.DataAlt(c), [v]), Core.App(Core.Var(specCall), Core.Var(v.var)))::alts
    |[] -> []     




and makeWrapperCall call : List<_> -> Core.Expr<Var, AnalysedVar<_>> = function
    |v::vs ->
        Core.App(makeWrapperCall call vs, Core.Var v.var)
    |[] -> Core.Var (call.var)


        
let rec wwTransform  = function
    |(b, Core.TopExpr e)::xs when isBuiltIn b ->
        (b, Core.TopExpr e)::wwTransform xs
    |(b, Core.TopExpr (e:Core.Expr<Var, AnalysedVar<_>>))::xs -> 
        let workerVar = {b with var = {b.var with Vars.unique = Worker b.var.unique}}
        let wrapperE, extraProgram = makeWrapper workerVar [] e
        let wrapper = b, Core.TopExpr(wrapperE)
        let worker = workerVar, Core.TopExpr e
        List.concat[[worker;wrapper]; extraProgram; wwTransform xs]
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
    | Core.Prim (w, es) -> Core.Prim(w, es |> List.map (renameBoundVarsInExpr env f))
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
        simplifyCase inlineMap ((e, b, alts) |> caseOfCase)
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
    | Core.Prim (p, es) -> 
        Core.Prim(p, es |> List.map (simplifyExpr inlineMap))
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
        let safeToInline = 
            match e with
            |Core.Case(e, b2, (alts, def)) ->
                match alts, def with
                |[], Core.Case(_) -> false
                |[x], Core.Case(_) -> false
                |_ -> true
            |_ -> 
                false
        let newRhs = simplifyExpr inlineMap rhs 
        // Inline small values
        if safeToInline || isSmall newRhs then
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
        match alts |> List.tryPick(function ((Core.DataAlt v2, [bs1]), e) when v = v2 -> Some(bs1, e) |_ -> None) with
        |Some(bs1, altE)  -> 
            if value |> isSmall then
                simplifyExpr (Map.ofList [bs1.var, value; b.var, Core.App(Core.Var(v), value)]) altE
            else
                Core.Case(newE, b, ([((Core.DataAlt v, [bs1]), altE)], Core.Unreachable))
        |None ->  
            if value |> isSmall then
                simplifyExpr (Map.ofList [b.var, Core.App(Core.Var(v), value)]) def
            else
                Core.Case(newE, b, ([], def))
    |value, v, ([], _) when value |> isSmall ->
        match def with
        |Core.Prim _ ->
            // The case around prim is needed 
            Core.Case(value, v, (alts, newDef))
        |_ -> simplifyExpr (Map.ofList [v.var, value]) newDef
    |_  -> Core.Case(newE, b, (newAlts, newDef))

and caseOfCase = function
    |(Core.Case(e2, b2, (alts2, def2)), b, (alts, def)) ->
        let newAlts2 = alts2 |> List.map (fun (p, e) -> (p, Core.Case(e, b, (alts, def))))
        let newDef = def2 |> fun def -> (Core.Case(def, b, (alts, def)))
        (e2, b2, (newAlts2, newDef)) |> caseOfCase
    |x -> x

and isConstr = function
    |Core.Var{callType=Some ConstrCall} -> true
    |_ -> false
    

let rec simplify inlineMap = function
    |(b, Core.TopExpr e)::xs -> 
        (b, Core.TopExpr (simplifyExpr inlineMap e))::simplify inlineMap xs
    |(v, Core.TopConstr vs)::xs -> 
        (v, Core.TopConstr vs)::simplify inlineMap xs
    |[] -> 
        []

let isWrapper b =
    match b.var.unique with
    |BuiltIn _ -> false
    |Worker (BuiltIn _) -> false
    |Worker (_) -> false
    |Specialized(_) -> false
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
    let builtinInlineMap = getTopInlineMap isBuiltIn analysis
    let simplified = simplify builtinInlineMap analysis
    let ww = simplified |> wwTransform
    let wrapperInlineMap = getTopInlineMap isWrapper ww
    let result = simplify wrapperInlineMap ww
    System.IO.File.WriteAllText("./Compiler/input.core", sprintf "%A" ww)
    System.IO.File.WriteAllText("./Compiler/output.core", sprintf "%A" result)
    result