module Analysis

type Args = int

type Strictness =
    | Lazy
    | Strict of Args
    | HyperStrict

type ArgStrictness =
    | BottomArgStrictness
    | ArgsStrictness of Strictness * ArgStrictness
    | TopArgStrictness

type DefaultMap<'k, 'v when 'k: comparison> = 'v * Map<'k, 'v>

type StrictnessResult =
    { args: ArgStrictness
      frees: DefaultMap<Core.UniqueName, Strictness> }

type StrictnessValue<'v when 'v: comparison> =
    { arity: int
      strictness: StrictnessResult}

type Analysis = {
    strictness: Strictness
}
let noAnalysis = {strictness=Lazy}

type VarFlags =
    {isWrapper:bool}
let noFlags = {isWrapper=false}

[<StructuredFormatDisplay("{AsString}")>]
type AnalysedVar<'v when 'v: comparison> = 
    {var:'v; analysis:Analysis; flags:VarFlags} 
    member m.AsString = ""
let unanalysed typ = {var={Vars.v=None;Vars.typ=typ;Vars.hintInline=false}; analysis=noAnalysis; flags=noFlags}
    

let rec normalizeArgStrictness = function
    |TopArgStrictness -> TopArgStrictness
    |BottomArgStrictness -> BottomArgStrictness
    |ArgsStrictness(x, xs) ->
        match x, normalizeArgStrictness xs with
        |Lazy, TopArgStrictness -> TopArgStrictness
        |HyperStrict, BottomArgStrictness -> BottomArgStrictness
        |x, xs -> ArgsStrictness(x, xs)


let lookup k (d, map) =
    match map |> Map.tryFind k with
    | None -> d
    | Some v -> v

let lookupAnalysis name k map =
    {var=k; analysis={strictness=lookup name map}; flags=noFlags}

let update k v (d, map) = 
    if v=d then
        (d, map |> Map.remove k)
    else
        (d, map |> Map.add k v)
let remove k (d, map) = (d, map |> Map.remove k)
let rec removeAll ks map = 
    match ks with
    |[] -> map
    |k::ks -> removeAll ks (remove k map)
let rec updateAll ks map = 
    match ks with
    |[] -> map
    |(k,v)::ks -> updateAll ks (update k v map)

let orStrictness =
    function
    | HyperStrict, x -> x
    | x, HyperStrict -> x
    | Strict(n1), Strict(n2) -> Strict(min n1 n2)
    | Lazy, _ -> Lazy
    | _, Lazy -> Lazy

let andStrictness =
    function
    | Lazy, x -> x
    | x, Lazy -> x
    | Strict(n1), Strict(n2) -> Strict(max n1 n2)
    | HyperStrict, _ -> HyperStrict
    | _, HyperStrict -> HyperStrict

let rec orArgs =
    function
    | BottomArgStrictness, x -> x
    | x, BottomArgStrictness -> x
    | ArgsStrictness(x1, y1), ArgsStrictness(x2, y2) -> ArgsStrictness(orStrictness (x1, x2), orArgs (y1, y2))
    | TopArgStrictness, _ -> TopArgStrictness
    | _, TopArgStrictness -> TopArgStrictness

let rec andArgs =
    function
    | TopArgStrictness, x -> x
    | x, TopArgStrictness -> x
    | ArgsStrictness(x1, y1), ArgsStrictness(x2, y2) -> ArgsStrictness(orStrictness (x1, x2), orArgs (y1, y2))
    | BottomArgStrictness, _ -> BottomArgStrictness
    | _, BottomArgStrictness -> BottomArgStrictness

let combineFrees combine ((d1, map1), (d2, map2)) =
    let getKeys = Map.toList >> List.map fst

    let keys =
        List.concat
            [ getKeys map1
              getKeys map2 ]
        |> List.distinct
    let def = combine (d1, d2)
    let map =
        keys
        |> List.map (fun key -> key, combine ((d1, map1) |> lookup key, (d2, map2) |> lookup key))
        |> List.filter(fun (_, value) -> value<>def)
        |> Map.ofList

    def, map

let orFrees frees = combineFrees orStrictness frees
let andFrees frees = combineFrees andStrictness frees

let orResult a b =
    { args = orArgs (a.args, b.args)
      frees = orFrees (a.frees, b.frees) }

let andResult a b =
    { args = andArgs (a.args, b.args)
      frees = andFrees (a.frees, b.frees) }

let defaultStrictness =
    { args = TopArgStrictness
      frees = (Lazy, Map.empty) }

let maxStrictnessResult =       
    { args = BottomArgStrictness
      frees = (HyperStrict, Map.empty) }
     
let maxStrictnessValue =       
    { arity = 0
      strictness = maxStrictnessResult }

let evaluate incomingArity value =
    match value with
    | Some { arity = arity; strictness = strictness } when incomingArity >= arity -> strictness
    | _ -> defaultStrictness

let rec manifestArity =
    function
    | Core.LamE(_, e) -> 1 + manifestArity e
    | _ -> 0

let rec analyseExpr (env: Map<Core.UniqueName, _>) incomingArity : 
    Core.Expr<_> -> StrictnessResult * Core.Expr<AnalysedVar<_>> =
    function
    | Core.VarE (name, v) -> 
        let innerResult = evaluate incomingArity (env |> Map.tryFind name)
        let unitResult = {
            frees=(Lazy, Map.ofList[name, Strict incomingArity])
            args=TopArgStrictness
        } 
        unitResult |> andResult innerResult,
        Core.varS (name)      
    | Core.Lit l -> 
        defaultStrictness,
        Core.Lit l
    | Core.LamE((name, x), e) ->
        let innerResult, innerExpr =
            match incomingArity with
            | 0 -> defaultStrictness, Core.mapExpr (fun v -> {var=v; analysis=noAnalysis; flags=noFlags}) e
            | _ -> analyseExpr env (incomingArity - 1) e

        let argStrictness = innerResult.frees |> lookup name
        let varAnalysis = innerResult.frees |> lookupAnalysis name x
        let frees = innerResult.frees |> remove name
        let args = ArgsStrictness(argStrictness, innerResult.args) |> normalizeArgStrictness        
        { args = args
          frees = frees },
        Core.lamE((name, varAnalysis), innerExpr)

    | Core.LetE(Core.NonRec, bs, e) ->
            let analysis, binds, eExpr = analyseNonRec env incomingArity e bs
            analysis, 
            Core.letE(Core.NonRec, binds, eExpr)
      | Core.LetE( Core.Join, bs, e) -> 
            let analysis, binds, eExpr = analyseNonRec env incomingArity e bs
            analysis, 
            Core.letE(Core.Join, binds, eExpr)
      | Core.LetE( Core.Rec, bs , e) -> 
            let analysis, binds, eExpr = analyseRec env incomingArity e bs
            analysis, 
            Core.letE(Core.Rec, binds, eExpr)
    | Core.CaseE(e, (name, v), alts) -> analyseCase env incomingArity e name v alts
    | Core.App(a, b) ->
        let aResult, aExpr = analyseExpr env (incomingArity + 1) a

        let argStrictness, argsStrictness =
            match aResult.args with
            | TopArgStrictness -> Lazy, TopArgStrictness
            | ArgsStrictness(x, xs) -> x, xs
            | BottomArgStrictness -> HyperStrict, BottomArgStrictness

        let bResult, bExpr =
            match argStrictness with
            | Lazy -> defaultStrictness, Core.mapExpr (fun v -> {var=v; analysis=noAnalysis; flags=noFlags}) b
            | Strict n -> analyseExpr env n b
            | HyperStrict -> analyseExpr env System.Int32.MaxValue b

        {aResult with args=argsStrictness} |> andResult bResult,
        Core.App(aExpr, bExpr)
    | Core.Prim (w, es) -> 
        let innerResult, innerEs = analysePrims env es
        innerResult,
        Core.Prim (w, innerEs)
    | Core.Unreachable -> maxStrictnessResult, Core.Unreachable

and analyseNonRec env incomingArity e =
    function
    | ((name, b), rhs)::xs ->
        let arity = manifestArity rhs
        let rhsResult, rhsExpr = analyseExpr env arity rhs
        let newEnv =
            env
            |> Map.add name
                   { arity = arity
                     strictness = rhsResult }
        let innerResult, nextResult, innerExpr = analyseNonRec newEnv incomingArity e xs
        let analysisVar = innerResult.frees |> lookupAnalysis name b
        let frees = innerResult.frees |> remove name
        { innerResult with frees = frees }, (((name, analysisVar), rhsExpr)::nextResult), innerExpr
    |[] -> 
        let innerResult, innerExpr = analyseExpr env incomingArity e
        innerResult, [], innerExpr

and analyseRec env incomingArity e bs = 
    let newEnv = getRecEnv env bs
    let innerResult, innerExpr = analyseExpr newEnv incomingArity e
    let binds = bs |> List.map(fun ((name,v),rhs) ->
        let _, innerRhs = analyseRhs env rhs 
        ((name, innerResult.frees |> lookupAnalysis name v), innerRhs))
    let frees = innerResult.frees |> removeAll(bs|>List.map (fun ((name, v),_)->name))
    { innerResult with frees = frees }, binds, innerExpr


and getRecEnv env (bs) =
    let approximation =  bs |> List.map (fun b -> b, maxStrictnessValue)
    fixRec env [] approximation

and fixRec env bs approx =
    let newApprox = getNextApprox env bs approx
    if approx = newApprox then
        let newApproxMapping = (approx |> List.map (fun (((name, v), _), result) -> (name, result)))
        env 
            |> Map.toList
            |> List.append newApproxMapping
            |> Map.ofList
    else 
        fixRec env bs newApprox

and getNextApprox env (bs) = function
    |(((name, v), rhs), approx)::xs ->
        let (newEnv:Map<_, StrictnessValue<_>>) = env |> Map.add name approx
        getNextApprox newEnv (((name, v), rhs)::bs) xs
    |[] ->
        analyseApprox env (bs|>List.rev)

and analyseApprox (env:Map<Core.UniqueName, StrictnessValue<_>>) = function
    |((name, v),rhs)::bs ->
        let rhsValue, _ = analyseRhs env rhs
        (((name, v), rhs), rhsValue)::analyseApprox env bs
    | [] -> []

and analyseRhs env rhs :StrictnessValue<_> * Core.Expr<AnalysedVar<_>> =
    let arity = manifestArity rhs
    let rhsResult, rhsExpr = analyseExpr env arity rhs
    {arity=arity; strictness=rhsResult}, rhsExpr


and analyseCase env incomingArity e name b (alts) =
    let innerResult, innerExpr = analyseExpr env 0 e
    let altsResult, newAlts = analyseAlts env incomingArity alts
    let analysisVar = innerResult.frees |> lookupAnalysis name b
    let frees = innerResult.frees |> remove name
    altsResult |> andResult {innerResult with frees=frees},
    Core.caseE(innerExpr, (name, analysisVar), (newAlts))

and analyseAlts env incomingArity =
    function
    | (pat, vs, e) :: alts ->
        let innerResult, innerExpr = analyseExpr env incomingArity e
        let analysedVars = vs |> List.map(fun (name, v) -> name, innerResult.frees |> lookupAnalysis name v)
        let frees = innerResult.frees |> removeAll (vs |> List.map fst)
        let altsResult, newAlts = analyseAlts env incomingArity alts
        { innerResult with frees = frees } |> orResult altsResult,
        (pat, analysedVars, innerExpr)::newAlts
    | [] -> maxStrictnessResult, []

and analysePrims env =
    function
    | e :: es ->
        let esResult, innerEs = analysePrims env es
        let eResult, innerE = analyseExpr env 0 e
        esResult |> andResult eResult,
        innerE::innerEs
    | [] -> defaultStrictness, []


let rec analyseTopConstrs constrs = function
    |((name, v), (c, vs))::xs -> 
        let program = analyseTopConstrs ((v,vs)::constrs) xs
        let analysedVars = vs |> List.map (fun (name, v) -> name, {var=v; analysis=noAnalysis; flags=noFlags})
        ((name, {var=v; analysis=noAnalysis; flags=noFlags}), (c, analysedVars))::program
    |[] -> 
        []

let rec analyseTopExprs binds = function
    |((name, b), (export, e))::xs -> 
        let env, program = analyseTopExprs (((name, b), e)::binds) xs
        let _, innerRhs = analyseRhs env (e:Core.Expr<_>) 
        env,
        ((name, {var=b; analysis=noAnalysis; flags=noFlags}), (export, innerRhs))::program
    |[] -> 
        getRecEnv Map.empty binds,
        []

let analyse (Core.Program e) = 
    let constrs = analyseTopConstrs [] e.constrs 
    let _env, exprs = analyseTopExprs [] e.exprs
    Core.closeProgram {
        constrs=constrs
        exprs=exprs
    }

