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