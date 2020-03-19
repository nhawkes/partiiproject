module Transform

open Analysis
open Vars
open Core
open Types

let isBuiltIn b =
    match b.var with
    | var -> var.hintInline

let combineMaps map1 map2 =
    let getKeys = Map.toList >> List.map fst

    let keys =
        List.concat
            [ getKeys map1
              getKeys map2 ]
        |> List.distinct

    let map =
        keys
        |> List.map (fun key ->
            key,
            List.append
                (map1
                 |> Map.tryFind key
                 |> Option.defaultValue [])
                (map2
                 |> Map.tryFind key
                 |> Option.defaultValue [])
            |> List.distinctBy fst)
        |> Map.ofList

    map

let rec getArgSpecializations args =
    function
    | Core.Var v -> Map.empty
    | Core.Const l -> Map.empty
    | Core.LamE(v, e) -> getArgSpecializations args e
    | Core.LetE(l, bs, e) -> getArgSpecializations args e
    | Core.CaseE(Core.VarS(v), _, alts) when args |> List.contains v ->
        let map =
            Map.ofList
                [ v,
                  alts
                  |> List.choose (function
                      | (Constr p, xs, _) -> Some(p, xs)
                      | _ -> None) ]
        alts
        |> List.map (fun (_, _, e) -> getArgSpecializations args e)
        |> List.fold combineMaps map
    | Core.CaseE(_, b, ([ _, _, e ])) -> getArgSpecializations args e
    | Core.CaseE(_, b, _) -> Map.empty
    | Core.App(a, b) -> getArgSpecializations args b
    | Core.Unreachable -> Map.empty

type ResultSpec =
    | TopResultSpec
    | ResultSpec of UniqueName * Typ
    | BotResultSpec

let rec getResultSpecialization (constrs: TopConstr<_> list) recCall =
    function
    | Core.Var v -> TopResultSpec
    | Core.Const l -> TopResultSpec
    | Core.LamE(v, e) -> getResultSpecialization constrs recCall e
    | Core.LetE(l, bs, e) -> getResultSpecialization constrs recCall e
    | Core.CaseE(_, b, (alts)) ->
        let altsE = alts |> List.map (fun (_, _, e) -> e)

        let result =
            altsE
            |> List.map (getResultSpecialization constrs recCall)
            |> List.fold (fun x y ->
                match x, y with
                | TopResultSpec, _ -> TopResultSpec
                | _, TopResultSpec -> TopResultSpec
                | ResultSpec(v, t), ResultSpec(v2, t2) when v = v2 -> ResultSpec(v, t)
                | BotResultSpec, x -> x
                | x, BotResultSpec -> x
                | _ -> TopResultSpec) BotResultSpec
        result
    | Core.App(Core.VarS(name), b) ->
        if name = recCall then
            BotResultSpec
        else
            match constrs
                  |> List.tryPick (function
                      | ((name2, _), (c, [ ts ])) when name = name2 ->
                          ts
                          |> (fun (_, x) -> x.var.typ)
                          |> Some
                      | _ -> None) with
            | Some t -> ResultSpec(name, t)
            | None -> TopResultSpec
    | Core.App(a, b) -> TopResultSpec
    | Core.Unreachable -> BotResultSpec


let rec makeWrapper constrs call vs =
    function
    | Core.LamE(v, e) -> makeWrapper constrs call (v :: vs) e
    | e ->
        let toSpecialize = getArgSpecializations (vs |> List.map (fun (name, _) -> name)) e
        let argSpecializations =
            vs |> List.map (function
                      | (name, v) ->
                          match toSpecialize |> Map.tryFind name with
                          | Some([ (c), [ value ] ]) -> (name, v), Some(c, value)
                          | _ -> (name, v), None)
        let specResult = getResultSpecialization constrs call e
        let specCall = freshen fvExpr [ e ] "spec_call"

        let extraProgram =
            if argSpecializations
               |> List.map snd
               |> List.forall (Option.isNone) then
                []
            else
                let resultT =
                    match specResult with
                    | ResultSpec(x, t) -> t
                    | _ -> ValueT

                let typ =
                    Types.createFuncT
                        (argSpecializations
                         |> List.map (function
                             | (name, v), Some(c, value) -> (snd value).var.typ
                             | (name, v), None -> ValueT)) resultT

                [ ((specCall, unanalysed typ),
                   (NoExport,
                    makeSpecialization (freshSource fvExpr []) constrs specResult e (argSpecializations |> List.rev))) ]

        let specializations = Some(argSpecializations, specCall, specResult, [])
        makeWrapperLambda call specializations [] (vs |> List.rev), extraProgram

and makeSpecialization (freshSource: FreshSource) (constrs: TopConstr<_> list) specResult e =
    function
    | ((name, v), Some(c, value)) :: xs ->
        let lam = freshSource.next "lam"
        let ((constrVar, _), _) = constrs |> List.find (fun (_, (c2, _)) -> c = c2)
        Core.lamE
            ((lam, snd value),
             Core.caseE
                 (Core.App(Core.varS (constrVar), Core.varS (lam)), (name, v),
                  ([ Core.DefAlt, [], makeSpecialization freshSource (constrs: TopConstr<_> list) specResult e xs ])))

    | ((name, v), None) :: xs ->
        Core.lamE ((name, v), makeSpecialization freshSource (constrs: TopConstr<_> list) specResult e xs)
    | [] ->
        match specResult with
        | ResultSpec(res, t) ->
            let (_, (resConstr, _)) = constrs |> List.find (fun ((res2, _), (_, _)) -> res = res2)
            let bindVar = freshSource.next "bind"
            let resultVar = freshSource.next "result"
            Core.caseE
                (e, (bindVar, unanalysed ValueT),
                 [ Core.Constr resConstr, [ resultVar, unanalysed t ], Core.varS (resultVar) ])
        | _ -> e

and makeWrapperLambda call specializations lams =
    function
    | v :: vs -> Core.lamE (v, makeWrapperLambda call specializations (v :: lams) vs)
    | [] -> makeWrapperBind (freshSource fvExpr []) call specializations [] (lams |> List.rev)

and makeWrapperBind (freshSource: FreshSource) call specializations binds =
    function
    | (name, v) :: vs ->
        match v.analysis.strictness with
        | Strictness.Lazy -> makeWrapperBind freshSource call specializations (name :: binds) vs
        | Strict _ ->
            let specAlts =
                match specializations with
                | None -> []
                | Some(argSpecializations, specCall, specResult, specBinds) ->
                    specializationAlts freshSource call vs (name :: binds) specCall specResult specBinds
                        argSpecializations

            let var = freshSource.next "var"

            let altsA =
                List.concat
                    [ [ DefAlt, [], makeWrapperBind freshSource call None (var :: binds) vs ]
                      specAlts ]

            Core.caseE (Core.varE (name, v), (var, unanalysed v.var.typ), altsA)
        | HyperStrict ->
            let var = freshSource.next "var"
            Core.caseE
                (Core.varE (name, v), (var, unanalysed v.var.typ),
                 [ DefAlt, [], makeWrapperBind freshSource call specializations (var :: binds) vs ])
    | [] -> makeWrapperCall call (binds)

and specializationAlts (freshSource: FreshSource) call vs binds specCall specResult specBinds =
    function
    | (_, Some(c, (_, value))) :: argSpecializations ->
        let v = freshSource.next "v"

        let innerCall =
            if argSpecializations |> List.isEmpty then
                makeWrapperBind (freshSource: FreshSource) specCall
                    (Some(argSpecializations, specCall, TopResultSpec, v :: specBinds)) (v :: specBinds) vs
            else
                makeWrapperBind (freshSource: FreshSource) call
                    (Some(argSpecializations, specCall, TopResultSpec, v :: specBinds)) binds vs

        let eval = freshSource.next "eval"

        let inner =
            match specResult with
            | ResultSpec(res, t) ->
                Core.caseE
                    (innerCall, (eval, unanalysed ValueT), [ DefAlt, [], Core.App(Core.varS res, Core.varS (eval)) ])
            | _ -> innerCall

        [ Core.Constr(c), [ v, unanalysed value.var.typ ], inner ]
    | _ :: argSpecializations -> []
    | [] -> []


and makeWrapperCall call: List<_> -> Core.Expr<_> =
    function
    | v :: vs -> Core.App(makeWrapperCall call vs, Core.varS v)
    | [] -> Core.varS (call)

let nameFrom s (name, x) = sprintf "%s_%s" s name

let rec wwTransform constrs source =
    function
    | ((name, b), (export, e)) :: xs when isBuiltIn b -> ((name, b), (export, e)) :: wwTransform constrs source xs
    | ((name, b), (export, e)) :: xs ->
        let workerVar = source.next (nameFrom "worker" name)
        let wrapperE, extraProgram = makeWrapper constrs workerVar [] e
        let wrapper = (name, { b with flags = { b.flags with isWrapper = true } }), (export, wrapperE)
        let worker = (workerVar, unanalysed b.var.typ), (NoExport, e)
        List.concat
            [ [ worker; wrapper ]
              extraProgram
              wwTransform constrs source xs ]
    | [] -> []


let rec isSmall constrs =
    function
    | Core.Unreachable
    | Core.Var _ -> true
    | Core.LamE(_, Core.Unreachable) -> true
    | Core.Const _ -> true
    | Core.App(VarS name, b) when constrs |> Map.containsKey name -> true
    | _ -> false

let rec simplifyExpr constrs =
    function
    | Core.VarS v -> Core.varS v
    | Core.Const l -> Core.Const l
    | Core.LamE(v, e) -> Core.lamE (v, simplifyExpr constrs e)
    | Core.LetE(l, bs, e) -> simplifyBinds constrs e (l, bs)
    | Core.CaseE(e, b, alts) ->
        let (newE, newB, newAlts) = (simplifyExpr constrs e, b, simplifyAlts constrs alts)
        let after = simplifyCase constrs (((newE, newB, newAlts)) |> caseOfCase)
        after
    | Core.App(a, b) ->
        let newA = simplifyExpr constrs a
        let newB = simplifyExpr constrs b
        match newA with
        | Core.LamE((name, v), e) ->
            let sub = (substExpr name newB e)
            let res = simplifyExpr constrs sub
            res
        | Core.LamE(v, e) -> Core.letE (Core.NonRec, [ v, b ], e) |> simplifyExpr constrs
        | Core.LetE(l, bs, e) -> Core.letE (l, bs, Core.App(e, b)) |> simplifyExpr constrs
        | Core.CaseE(e, v, ([ p, ps, def ])) -> Core.caseE (e, v, [ p, ps, Core.App(def, b) ]) |> simplifyExpr constrs
        | _ -> Core.App(newA, newB) |> factorApp constrs
    | Core.Unreachable -> Core.Unreachable

and factorApp constrs =
    function
    | Core.App(f, Core.CaseE(e, b, ([ p, xs, pe ]))) when f |> isSmall constrs ->
        let after = Core.caseE (e, b, ([ p, xs, Core.App(f, pe) |> factorApp constrs ]))
        after

    | x -> x



and simplifyAlts constrs (alts) =
    let newAlts =
        alts
        |> List.map (fun (p: Const, ps: Binder<AnalysedVar<_>> list, e: Expr<_>) -> (p, ps, simplifyExpr constrs e))
    (newAlts)



and simplifyBinds constrs e =
    function
    | Core.NonRec, [ ((name, v), rhs) ] ->
        let newRhs = simplifyExpr constrs rhs
        // Inline small values
        if isSmall constrs newRhs then
            simplifyExpr constrs (substExpr name newRhs e)
        else if v.analysis.strictness <> Lazy then
            // Change strict expressions into a case
            Core.caseE (newRhs, (name, v), [ Core.DefAlt, [], simplifyExpr constrs e ])
        else
            Core.letE (Core.NonRec, [ (name, v), newRhs ], (simplifyExpr constrs e))
    | Core.Join, [ ((name, b1), rhs) ] ->
        let safeToInline =
            match e with
            | Core.Case(e, b2, (alts)) -> true
            | _ -> false

        let newRhs = simplifyExpr constrs rhs
        // Inline small values
        if (safeToInline || isSmall constrs newRhs)
        then simplifyExpr constrs (substExpr name rhs e)
        else Core.letE (Core.Join, [ (name, b1), newRhs ], (simplifyExpr constrs e))
    | Core.NonRec, [] -> simplifyExpr constrs e
    | Core.Rec, [] -> simplifyExpr constrs e
    | Core.Rec, bs ->
        Core.letE
            (Core.Rec, (bs |> List.map (fun (v, rhs) -> v, simplifyExpr constrs rhs)), (simplifyExpr constrs e))
    | Core.Join, xs -> Core.letE (Core.Join, xs, (simplifyExpr constrs e))

and simplifyCase constrs (e, (name, b), (alts)) =
    let newE = simplifyExpr constrs e
    let (newAlts) = simplifyAlts constrs (alts)
    match newE, b, (newAlts) with
    | value, b, ([ DefAlt, [], def ]) when value |> isSmall constrs ->
        match def with
        | _ -> simplifyExpr constrs (substExpr name value def)
    | Core.App(Core.VarS(v), value), b, (alts) when constrs |> Map.containsKey v ->
        let c = constrs |> Map.find v
        match alts
              |> List.tryPick (function
                  | (Core.Constr c2, [ bs1 ], e) when c = c2 -> Some(bs1, e)
                  | _ -> None) with
        | Some((name1, bs1), altE) ->
            if value |> isSmall constrs then
                simplifyExpr constrs
                    (altE
                     |> substExpr name1 value
                     |> substExpr (name) (App(varS v, value)))
            else
                Core.caseE (newE, (name, b), ([ (Core.Constr c, [ (name1, bs1) ], altE) ]))
        | None -> Core.caseE (newE, (name, b), newAlts)
    | _ -> Core.caseE (newE, (name, b), (newAlts))

and caseOfCase =
    function
    | (Core.CaseE(e2, (name2, b2), (alts2)), b, (alts)) as before ->
        let freshB2 =
            freshen fvExpr
                (List.concat
                    [ alts |> List.map (fun (p, ps, e) -> e)
                      alts2 |> List.map (fun (p, ps, e) -> e) ]) (name2 |> fst)

        let newAlts2 =
            alts2
            |> List.map (fun (p, ps, e) -> (p, ps, Core.caseE (substExpr (name2) (varS freshB2) e, b, (alts))))
        let after = (e2, (freshB2, b2), (newAlts2))
        after |> caseOfCase
    | x -> x



let rec simplify constrs =
    function
    | ((name, b), (export, e)) :: xs -> ((name, b), (export, simplifyExpr constrs e)) :: simplify constrs xs
    | [] -> []



let rec getTopInlineMap shouldInline =
    function
    | ((name, v), (export, e)) :: xs ->
        if shouldInline v
        then getTopInlineMap shouldInline xs |> Map.add name e
        else getTopInlineMap shouldInline xs
    | [] -> Map.empty

let isWrapper { flags = flags } = flags.isWrapper


let transform optimise (program: ClosedProgram<Var>) =
    let (Program analysis) = program |> Analysis.analyse
    let builtinInlineMap = getTopInlineMap isBuiltIn analysis.exprs

    let constrs =
        analysis.constrs
        |> List.map (fun ((name, b), (c, _)) -> name, c)
        |> Map.ofList
    let inlined =
        builtinInlineMap
        |> Map.toList
        |> List.fold (fun expr (x, v) -> substTopExprs x v expr) analysis.exprs

    let simplified = simplify constrs inlined
    let freshSource = (freshSource fvProgram [ closeProgram analysis ])
    let ww = simplified |> wwTransform analysis.constrs freshSource
    let wrapperInlineMap = getTopInlineMap isWrapper ww

    let wrapperInlined =
        wrapperInlineMap
        |> Map.toList
        |> List.fold (fun expr (x, v) -> substTopExprs x v expr) ww

    let result = simplify constrs wrapperInlined
    if optimise
    then closeProgram { analysis with exprs = result }
    else closeProgram analysis
