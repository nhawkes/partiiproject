module Transform

open Analysis
open Vars
open Core

let isBuiltIn b =
    match b.var with
    |Some var -> var.hintInline
    |_ -> false

let rec getArgSpecializations args = function
    | Core.Var v -> Map.empty
    | Core.Lit l -> Map.empty
    | Core.LamE(v, e) -> getArgSpecializations args e
    | Core.LetE(l, bs, e) -> 
        getArgSpecializations args e
    | Core.CaseE(Core.VarS(v), _, alts) when args |> List.contains v ->
        Map.ofList[v, alts |> List.map(fun (p, xs, _) -> p, xs)]
    | Core.CaseE(_, b, ([_, _, e])) -> 
        getArgSpecializations args e
    | Core.App(a, b) -> 
        getArgSpecializations args b    

type ResultSpec =
    |TopResultSpec
    |ResultSpec of UniqueName
    |BotResultSpec
    
let rec getResultSpecialization recCall = function
    | Core.Var v -> TopResultSpec
    | Core.Lit l -> TopResultSpec
    | Core.LamE(v, e) -> getResultSpecialization recCall e
    | Core.LetE(l, bs, e) -> 
        getResultSpecialization recCall e
    | Core.CaseE(_, b, (alts)) -> 
        let altsE = alts |> List.map (fun (_,_,e) -> e)
        let result = 
            altsE |> 
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
    | Core.App(Core.VarE (name, _), b) -> 
        if name = recCall then
            BotResultSpec
    // TODO        
    //    else if v.callType = Some ConstrCall then
    //        ResultSpec v
        else
            TopResultSpec
    | Core.Unreachable -> BotResultSpec


let rec makeWrapper constrs call vs = function
    |Core.LamE(v, e) -> makeWrapper constrs call (v::vs) e
    |e ->
        let toSpecialize = getArgSpecializations (vs |> List.map (fun (name, _) -> name)) e
        let specializations, extraProgram =
            match vs with
            |[] -> Map.empty, []
            |(name, v)::_ ->
                match toSpecialize |> Map.tryFind name with
                |Some([Core.DataAlt(c), [value]]) ->
                    let specCall = freshen fvExpr [e] "spec_call"
                    let specResult = getResultSpecialization call e
                    Map.ofList [(name), [c, value, specCall, specResult]], [((specCall, unanalysed), (makeSpecialization constrs specResult e [(name, v),c, value]))]
                |_ -> Map.empty, []
        makeWrapperLambda call specializations [] (vs |> List.rev), extraProgram

and makeSpecialization constrs specResult e = function
    |[v:Binder<_>, c, (valueName, value)] ->
        let lam = Core.freshen Core.fvExpr [e] "lam"     
        let (constrVar, _) = constrs |> List.find(fun (_, (c2, _)) -> c=c2)
        match specResult with
        |ResultSpec res ->
            let (_, (resConstr, _)) = constrs |> List.find(fun (res2, (_, _)) -> res=res2)
            let bindVar = Core.freshen Core.fvExpr [e] "bind"   
            let resultVar = Core.freshen Core.fvExpr [e] "result"
            NoExport, Core.lamE((lam, value), 
                Core.caseE(Core.App(Core.varS(constrVar), Core.varS(lam)), v, 
                  ([Core.DefAlt, [], 
                    Core.caseE(e, (bindVar, unanalysed),
                         [Core.DataAlt resConstr, [resultVar, unanalysed],  Core.varS(resultVar)])])                
             ))
        |_ -> 
            NoExport,  Core.lamE((lam, unanalysed), Core.caseE(Core.App(Core.varS(constrVar), Core.varS(lam)), v, ([DefAlt, [], e])))


and makeWrapperLambda call specializations lams = function
    |v::vs -> Core.lamE(v, makeWrapperLambda call specializations (v::lams) vs)
    |[] ->
        makeWrapperBind call specializations [] (lams |> List.rev)

and makeWrapperBind call specializations binds = function
    |(name, v)::vs -> 
        match v.analysis.strictness with
        |Strictness.Lazy ->
            makeWrapperBind call specializations (name::binds) vs
        |Strict _ ->
            let specAlts = 
                specializations |> Map.tryFind name |> Option.defaultValue [] |> specializationAlts

            let var = freshen fvAlts [] "var"
            let altsA = 
                List.concat [
                    [DefAlt, [], makeWrapperBind call specializations (var::binds) vs]
                    specAlts
                ]
            
            Core.caseE(Core.varE((name, v)), (var, unanalysed), altsA)
        |HyperStrict ->
            let var = freshen fvAlts [] "var"
            Core.caseE(Core.varS(var), (var, unanalysed), 
                [DefAlt, [], makeWrapperBind call specializations (var::binds) vs])
    |[] ->
        makeWrapperCall call (binds)

and specializationAlts : _ -> ((_*_*Expr<_>) list)  = function
    |(c, value, specCall, specResult)::specs ->
        let (alts:(_*_*Expr<_>) list) = specializationAlts specs
        let v = freshen fvExpr [] "v"
        let eval = freshen fvExpr [] "eval"
        match specResult with
        |ResultSpec res  ->
            (Core.DataAlt(c), [(v:UniqueName), unanalysed], 
                Core.caseE(Core.App(Core.varS(specCall), Core.varS(v)), (eval, unanalysed), [                    
                    DefAlt, [], Core.App(Core.varS res, Core.varS(eval))
                ] 
                
            ))           
            ::alts
        |_ ->
            (Core.DataAlt(c), [v, unanalysed], Core.App(Core.varS(specCall), Core.varS(v)))::alts
    |[] -> ([]:(_*_*Expr<_>) list)  




and makeWrapperCall call : List<_> -> Core.Expr<_> = function
    |v::vs ->
        Core.App(makeWrapperCall call vs, Core.varS v)
    |[] -> Core.varS (call)


        
let rec wwTransform constrs source = function
    |((name, b), (export, e))::xs when isBuiltIn b ->
        ((name, b), (export, e))::wwTransform constrs source xs
    |((name, b), (export, e))::xs -> 
        let workerVar = source.next "worker"
        let wrapperE, extraProgram = makeWrapper constrs workerVar [] e
        let wrapper = (name, b), (NoExport, wrapperE)
        let worker = (workerVar, unanalysed), (export, e)
        List.concat[[worker;wrapper]; extraProgram; wwTransform constrs source xs]
    |[] -> 
        []




(*

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

*)


let rec isSmall = function
    |Core.Unreachable
    |Core.Var _ -> true
    |Core.LamE(_, Core.Unreachable) -> true
    |Core.Lit _ -> true
    |_ -> false

let rec simplifyExpr (inlineMap:Map<UniqueName, Core.Expr<AnalysedVar<_>>>) = function    
    | Core.VarS v -> 
        match inlineMap |> Map.tryFind v with
        |Some e -> 
            e
        |None ->
            Core.varS v
    | Core.Lit l -> Core.Lit l
    | Core.LamE(v, e) -> Core.lamE(v, simplifyExpr inlineMap e)
    | Core.LetE(l, bs, e) -> 
        simplifyBinds inlineMap e (l, bs) 
    | Core.Case(e, b, alts) -> 
        simplifyCase inlineMap ((e, b, alts) |> caseOfCase)
    | Core.App(a, b) -> 
        let newA = simplifyExpr inlineMap a
        let newB = simplifyExpr inlineMap b
        match newA with
        |Core.LamE((name, v), e) -> 
            simplifyExpr (Map.ofList [name, newB]) e
        |Core.LamE(v, e) when false ->
            Core.letE(Core.NonRec, [v, b], e) |> simplifyExpr inlineMap 
        |Core.LetE(l, bs, e) when false ->
            Core.letE(l, bs, Core.App(e, b)) |> simplifyExpr inlineMap
        |Core.CaseE(e, v, ([Core.DefAlt, [], def])) when false ->
            Core.caseE(e, v, [Core.DefAlt, [], Core.App(def, b)]) |> simplifyExpr inlineMap
        |_ ->
        Core.App(newA, newB) |> factorApp
    | Core.Prim (p, es) -> 
        Core.Prim(p, es |> List.map (simplifyExpr inlineMap))
    | Core.Unreachable -> Core.Unreachable

and factorApp = function
    |Core.App(f, Core.CaseE(e, b, ([p, xs, pe]))) when f |> isSmall ->
        Core.caseE(e, b, ([p, xs, Core.App(f, pe) |> factorApp]))
    |x -> x



and simplifyAlts inlineMap (alts, def) = 
    let newAlts = alts |> List.map(fun (p,e) -> (p, simplifyExpr inlineMap e))
    let newDef = simplifyExpr inlineMap def    
    (newAlts, newDef)



and simplifyBinds inlineMap e = function
    |Core.NonRec, [((name, v), rhs)] ->
        let newRhs = simplifyExpr inlineMap rhs 
        // Inline small values
        if false && isSmall newRhs then
            printfn "Inlining: %A" newRhs
            let newsimplifyMap = inlineMap |> Map.add name newRhs
            simplifyExpr newsimplifyMap e
        else if false && v.analysis <> Lazy then
            printfn "Let-to-case: %A" newRhs            
            // Change strict expressions into a case
            Core.caseE(newRhs, (name, v), [Core.DefAlt, [], simplifyExpr inlineMap e])
        else
            Core.letE(Core.NonRec, [(name, v), newRhs], (simplifyExpr inlineMap e))
    |Core.Join, [((name, b1), rhs)] -> 
        let safeToInline = 
            match e with
            |Core.Case(e, b2, (alts)) ->
                match alts with
                |[x] -> false
                |_ -> true
            |_ -> 
                false
        let newRhs = simplifyExpr inlineMap rhs 
        // Inline small values
        if safeToInline || isSmall newRhs then
            let newInlineMap = inlineMap |> Map.add name newRhs
            simplifyExpr newInlineMap e
        else
            Core.letE(Core.Join, [(name, b1), newRhs], (simplifyExpr inlineMap e))
    |Core.Rec, [] -> simplifyExpr inlineMap e
    |Core.Rec, bs -> 
        Core.letE(Core.Rec, (bs |> List.map (fun (v,rhs) -> v, simplifyExpr inlineMap rhs)), (simplifyExpr inlineMap e))

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
        let newDef2 = (Core.Case(def2, b, (alts, def)))
        (e2, b2, (newAlts2, newDef2)) |> caseOfCase
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
