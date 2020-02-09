module Transform

open Analysis
open Vars
open Core
open Types

let isBuiltIn b =
    match b.var with
    |var -> var.hintInline

let rec getArgSpecializations args = function
    | Core.Var v -> Map.empty
    | Core.Lit l -> Map.empty
    | Core.LamE(v, e) -> getArgSpecializations args e
    | Core.LetE(l, bs, e) -> 
        getArgSpecializations args e
    | Core.CaseE(Core.VarS(v), _, alts) when args |> List.contains v ->
        Map.ofList[v, alts |> List.choose(function |(DataAlt p, xs, _) -> Some(p, xs) |_ -> None)]
    | Core.CaseE(_, b, ([_, _, e])) -> 
        getArgSpecializations args e
    | Core.App(a, b) -> 
        getArgSpecializations args b    

type ResultSpec =
    |TopResultSpec
    |ResultSpec of UniqueName * Typ list
    |BotResultSpec
    
let rec getResultSpecialization (constrs:TopConstr<_> list) recCall = function
    | Core.Var v -> TopResultSpec
    | Core.Lit l -> TopResultSpec
    | Core.LamE(v, e) -> getResultSpecialization constrs recCall e
    | Core.LetE(l, bs, e) -> 
        getResultSpecialization constrs recCall e
    | Core.CaseE(_, b, (alts)) -> 
        let altsE = alts |> List.map (fun (_,_,e) -> e)
        let result = 
            altsE |> 
            List.map (getResultSpecialization constrs recCall) |> 
            List.fold (fun x y ->
                match x,y with
                |TopResultSpec, _ -> TopResultSpec
                |_, TopResultSpec -> TopResultSpec
                |ResultSpec (v, t), ResultSpec (v2,t2) when v=v2 -> ResultSpec (v,t)
                |BotResultSpec, x -> x
                |x, BotResultSpec -> x
                |_ -> TopResultSpec
            ) BotResultSpec
        result        
    | Core.App(Core.VarS (name), b) -> 
        if name = recCall then
            BotResultSpec      
        else 
        match constrs |> List.tryPick(fun ((name2, _), (c, ts)) -> if name=name2 then ts |> List.map (fun (_,x) -> x.var.typ) |> Some else None) with
            |Some t -> ResultSpec (name, t)
            |None -> TopResultSpec
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
                |Some([(c), [value]]) ->
                    let specCall = freshen fvExpr [e] "spec_call"
                    let specResult = getResultSpecialization constrs call e
                    let resultT = match specResult with ResultSpec(_, [t]) -> t |_ -> ValueT
                    Map.ofList [(name), [c, value, specCall, specResult]], [((specCall, unanalysed (FuncT((snd value).var.typ, resultT))), (makeSpecialization constrs specResult e [(name, v),c, value]))]
                |_ -> Map.empty, []
        makeWrapperLambda call specializations [] (vs |> List.rev), extraProgram

and makeSpecialization (constrs:TopConstr<_> list) specResult e = function
    |[v:Binder<_>, c, (valueName, value)] ->
        let lam = Core.freshen Core.fvExpr [e] "lam"     
        let ((constrVar, _), _) = constrs |> List.find(fun (_, (c2, _)) -> c=c2)
        match specResult with
        |ResultSpec (res, [t]) ->
            let (_, (resConstr, _)) = constrs |> List.find(fun ((res2, _), (_, _)) -> res=res2)
            let bindVar = Core.freshen Core.fvExpr [e] "bind"   
            let resultVar = Core.freshen Core.fvExpr [e] "result"
            NoExport, Core.lamE((lam, value), 
                Core.caseE(Core.App(Core.varS(constrVar), Core.varS(lam)), v, 
                  ([Core.DefAlt, [], 
                    Core.caseE(e, (bindVar, unanalysed ValueT),
                         [Core.DataAlt resConstr, [resultVar, unanalysed t],  Core.varS(resultVar)])])                
             ))
        |_ -> 
            NoExport,  Core.lamE((lam, unanalysed ValueT), Core.caseE(Core.App(Core.varS(constrVar), Core.varS(lam)), v, ([DefAlt, [], e])))


and makeWrapperLambda call specializations lams = function
    |v::vs -> Core.lamE(v, makeWrapperLambda call specializations (v::lams) vs)
    |[] ->
        makeWrapperBind (freshSource fvExpr []) call specializations [] (lams |> List.rev)

and makeWrapperBind (freshSource:FreshSource) call specializations binds = function
    |(name, v)::vs -> 
        match v.analysis.strictness with
        |Strictness.Lazy ->
            makeWrapperBind freshSource call specializations (name::binds) vs
        |Strict _ ->
            let specAlts = 
                specializations |> Map.tryFind name |> Option.defaultValue [] |> specializationAlts freshSource

            let var = freshSource.next "var"
            let altsA = 
                List.concat [
                    [DefAlt, [], makeWrapperBind freshSource call specializations (var::binds) vs]
                    specAlts
                ]
            
            Core.caseE(Core.varE(name, v), (var, unanalysed v.var.typ), altsA)
        |HyperStrict ->
            let var = freshSource.next "var"
            Core.caseE(Core.varE(name, v), (var, unanalysed v.var.typ), 
                [DefAlt, [], makeWrapperBind freshSource call specializations (var::binds) vs])
    |[] ->
        makeWrapperCall call (binds)

and specializationAlts (freshSource:FreshSource) : _ -> ((_*_*Expr<_>) list)  = function
    |(c, (_, value), specCall, specResult)::specs ->
        let (alts:(_*_*Expr<_>) list) = specializationAlts freshSource specs
        let v = freshSource.next "v"
        let eval = freshSource.next "eval"
        match specResult with
        |ResultSpec (res, [t])  ->
            (Core.DataAlt(c), [(v:UniqueName), unanalysed t], 
                Core.caseE(Core.App(Core.varS(specCall), Core.varS(v)), (eval, unanalysed ValueT), [                    
                    DefAlt, [], Core.App(Core.varS res, Core.varS(eval))
                ] 
                
            ))           
            ::alts
        |_ ->
            (Core.DataAlt(c), [v, unanalysed value.var.typ], Core.App(Core.varS(specCall), Core.varS(v)))::alts
    |[] -> ([]:(_*_*Expr<_>) list)  




and makeWrapperCall call : List<_> -> Core.Expr<_> = function
    |v::vs ->
        Core.App(makeWrapperCall call vs, Core.varS v)
    |[] -> Core.varS (call)

let nameFrom s (name, x) = sprintf "%s_%s" s name
        
let rec wwTransform constrs source = function
    |((name, b), (export, e))::xs when isBuiltIn b ->
        ((name, b), (export, e))::wwTransform constrs source xs
    |((name, b), (export, e))::xs -> 
        let workerVar = source.next (nameFrom "worker" name)
        let wrapperE, extraProgram = makeWrapper constrs workerVar [] e
        let wrapper = (name, {b with flags={b.flags with isWrapper=true}}), (export, wrapperE)
        let worker = (workerVar, unanalysed b.var.typ) , (NoExport, e)
        List.concat[[worker;wrapper]; extraProgram; wwTransform constrs source xs]
    |[] -> 
        []


let rec isSmall constrs = function
    |Core.Unreachable
    |Core.Var _ -> true
    |Core.LamE(_, Core.Unreachable) -> true
    |Core.Lit _ -> true
    |Core.App (VarS name, b) when constrs |> Map.containsKey name ->  true
    |_ -> false

let rec simplifyExpr constrs = function
    | Core.VarS v -> Core.varS v
    | Core.Lit l -> Core.Lit l
    | Core.LamE(v, e) -> Core.lamE(v, simplifyExpr constrs e)
    | Core.LetE(l, bs, e) -> 
        simplifyBinds constrs e (l, bs) 
    | Core.CaseE(e, b, alts) -> 
        let (newE, newB, newAlts) = (simplifyExpr constrs e, b, simplifyAlts constrs alts)
        let after = simplifyCase constrs (((newE, newB, newAlts))|> caseOfCase)
        after
    | Core.App(a, b) -> 
        let newA = simplifyExpr constrs a
        let newB = simplifyExpr constrs b
        match newA with
        |Core.LamE((name, v), e) -> 
            let sub =  (substExpr name newB e)
            let res = simplifyExpr constrs sub
            res    
        |Core.LamE(v, e) when false ->
            Core.letE(Core.NonRec, [v, b], e) |> simplifyExpr constrs 
        |Core.LetE(l, bs, e) when false ->
            Core.letE(l, bs, Core.App(e, b)) |> simplifyExpr constrs
        |Core.CaseE(e, v, ([p, ps, def])) ->
            Core.caseE(e, v, [p, ps, Core.App(def, b)]) |> simplifyExpr constrs
        |_ ->
            Core.App(newA, newB)  |> factorApp constrs
    | Core.Prim (p, es) -> 
        Core.Prim(p, es |> List.map (simplifyExpr constrs))
    | Core.Unreachable -> Core.Unreachable

and factorApp constrs = function
    |Core.App(f, Core.CaseE(e, b, ([p, xs, pe]))) when f |> isSmall constrs ->
        let after = Core.caseE(e, b, ([p, xs, Core.App(f, pe) |> factorApp constrs]))
        after
        
    |x -> x



and simplifyAlts constrs (alts) = 
    let newAlts = alts |> List.map(fun (p:Pat, ps:Binder<AnalysedVar<_>> list,e : Expr<_>) -> (p, ps, simplifyExpr constrs e))
    (newAlts)



and simplifyBinds constrs e = function
    |Core.NonRec, [((name, v), rhs)] ->
        let newRhs = simplifyExpr constrs rhs 
        // Inline small values
        if false && isSmall constrs newRhs then
            printfn "Inlining: %A" newRhs
            simplifyExpr constrs (substExpr name newRhs e)
        else if false && v.analysis.strictness <> Lazy then
            printfn "Let-to-case: %A" newRhs            
            // Change strict expressions into a case
            Core.caseE(newRhs, (name, v), [Core.DefAlt, [], simplifyExpr constrs e])
        else
            Core.letE(Core.NonRec, [(name, v), newRhs], (simplifyExpr constrs e))
    |Core.Join, [((name, b1), rhs)] -> 
        let safeToInline = 
            match e with
            |Core.Case(e, b2, (alts)) ->
                match alts with
                |[x] -> true
                |_ -> true
            |_ -> 
                false
        let newRhs = simplifyExpr constrs rhs 
        // Inline small values
        if (safeToInline || isSmall constrs newRhs) then
            simplifyExpr constrs (substExpr name rhs e)
        else
            Core.letE(Core.Join, [(name, b1), newRhs], (simplifyExpr constrs e))
    |Core.Rec, [] -> simplifyExpr constrs e
    |Core.Rec, bs -> 
        Core.letE(Core.Rec, (bs |> List.map (fun (v,rhs) -> v, simplifyExpr constrs rhs)), (simplifyExpr constrs e))

and simplifyCase constrs (e, (name, b), (alts)) =
    let newE = simplifyExpr constrs e
    let (newAlts) = simplifyAlts constrs (alts)
    match newE, b, (newAlts) with
    |value, b, ([DefAlt, [], def]) when value |> isSmall constrs ->
        match def with
        |Core.Prim _ ->
            // The case around prim is needed 
            Core.caseE(value, (name, b), (alts))
        |_ -> simplifyExpr constrs (substExpr name value def)
    |Core.App(Core.VarS(v), value), b, (alts) when constrs |> Map.containsKey v ->  
        let c = constrs |> Map.find v      
        match alts |> List.tryPick(function (Core.DataAlt c2, [bs1], e) when c = c2 -> Some(bs1, e) |_ -> None) with
        |Some((name1, bs1), altE)  -> 
            if value |> isSmall constrs then
                simplifyExpr constrs (altE |> substExpr name1 value |> substExpr (name) (App (varS v, value)))
            else
                Core.caseE(newE, (name, b), ([(Core.DataAlt c, [(name1, bs1)], altE)]))
        |None ->  
                Core.caseE(newE, (name, b), newAlts)
    |_  -> 
        Core.caseE(newE, (name, b), (newAlts))

and caseOfCase = function
    |(Core.CaseE(e2, (name2, b2), (alts2)), b, (alts)) as before ->
        let freshB2 = freshen fvExpr (List.concat [alts |> List.map (fun (p, ps, e) -> e); alts2 |> List.map (fun (p, ps, e) -> e)]) (name2 |> fst)
        let newAlts2 = alts2 |> List.map (fun (p, ps, e) -> (p, ps, Core.caseE(substExpr (name2) (varS freshB2) e, b, (alts))))        
        let after = (e2, (freshB2, b2), (newAlts2))
        after |> caseOfCase
    |x ->
        x

    

let rec simplify constrs  = function
    |((name, b), (export, e))::xs -> 
        ((name, b), (export, simplifyExpr constrs e))::simplify constrs xs
    |[] -> 
        []



let rec getTopInlineMap shouldInline = function
    |((name, v), (export, e))::xs ->
        if shouldInline v then
            getTopInlineMap shouldInline xs |> Map.add name e
        else
            getTopInlineMap shouldInline xs
    |[] -> Map.empty

let isWrapper {flags=flags} = flags.isWrapper


let transform (program:ClosedProgram<Var>) =     
    let (Program analysis) = program |> Analysis.analyse
    let builtinInlineMap = getTopInlineMap isBuiltIn analysis.exprs
    let constrs = analysis.constrs |> List.map (fun ((name,b), (c,_)) -> name, c) |> Map.ofList 
    let inlined = builtinInlineMap |> Map.toList |> List.fold (fun expr (x,v) -> substTopExprs x v expr) analysis.exprs
    let simplified = simplify constrs inlined
    let freshSource = (freshSource fvProgram [closeProgram analysis])
    let ww = simplified |> wwTransform analysis.constrs freshSource
    let wrapperInlineMap = getTopInlineMap isWrapper ww
    let wrapperInlined = wrapperInlineMap |> Map.toList |> List.fold (fun expr (x,v) -> substTopExprs x v expr) ww
    let result = simplify constrs wrapperInlined
    System.IO.File.WriteAllText("./Compiler/input.core", (printProgram (closeProgram analysis)))
    System.IO.File.WriteAllText("./Compiler/inlined.core", (printProgram (closeProgram {analysis with exprs=inlined})))
    System.IO.File.WriteAllText("./Compiler/simplified.core", (printProgram (closeProgram {analysis with exprs=simplified})))
    System.IO.File.WriteAllText("./Compiler/ww.core", (printProgram (closeProgram {analysis with exprs=ww})))
    System.IO.File.WriteAllText("./Compiler/output.core", (printProgram (closeProgram {analysis with exprs=result})))
    closeProgram {analysis with exprs=result}
