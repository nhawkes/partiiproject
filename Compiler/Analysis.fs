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

type StrictnessResult<'v when 'v: comparison> =
    { args: ArgStrictness
      frees: DefaultMap<'v, Strictness> }

type StrictnessValue<'v when 'v: comparison> =
    { arity: int
      strictness: StrictnessResult<'v> }

type AnalysedVar<'v when 'v: comparison> = {var:'v; analysis:Strictness}  

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

let lookupAnalysis k map =
    {var=k; analysis=lookup k map}

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
    | Core.Lam(_, e) -> 1 + manifestArity e
    | _ -> 0

let rec analyseExpr (env: Map<_, _>) incomingArity : 
    Core.Expr<'v, 'v> -> StrictnessResult<'v> * Core.Expr<'v, AnalysedVar<'v>> =
    function
    | Core.Var v -> 
        let innerResult = evaluate incomingArity (env |> Map.tryFind v)
        let unitResult = {
            frees=(Lazy, Map.ofList[v, Strict incomingArity])
            args=TopArgStrictness
        } 
        unitResult |> andResult innerResult,
        Core.Var v       
    | Core.Lit l -> 
        defaultStrictness,
        Core.Lit l
    | Core.Lam(x, e) ->
        let innerResult, innerExpr =
            match incomingArity with
            | 0 -> defaultStrictness, Core.mapExpr (fun v -> {var=v; analysis=Lazy}) e
            | _ -> analyseExpr env (incomingArity - 1) e

        let argStrictness = innerResult.frees |> lookup x
        let varAnalysis = innerResult.frees |> lookupAnalysis x
        let frees = innerResult.frees |> remove x
        let args = ArgsStrictness(argStrictness, innerResult.args) |> normalizeArgStrictness        
        { args = args
          frees = frees },
        Core.Lam(varAnalysis, innerExpr)

    | Core.Let(bs, e) ->
        match bs with
        | Core.NonRec b -> 
            let analysis, binds, eExpr = analyseNonRec env incomingArity e b
            analysis, 
            Core.Let(Core.NonRec binds, eExpr)
        | Core.Join b -> 
            let analysis, binds, eExpr = analyseNonRec env incomingArity e b
            analysis, 
            Core.Let(Core.Join binds, eExpr)
        | Core.Rec bs -> 
            let analysis, binds, eExpr = analyseRec env incomingArity e bs
            analysis, 
            Core.Let(Core.Rec binds, eExpr)
    | Core.Case(e, v, alts) -> analyseCase env incomingArity e v alts
    | Core.App(a, b) ->
        let aResult, aExpr = analyseExpr env (incomingArity + 1) a

        let argStrictness, argsStrictness =
            match aResult.args with
            | TopArgStrictness -> Lazy, TopArgStrictness
            | ArgsStrictness(x, xs) -> x, xs
            | BottomArgStrictness -> HyperStrict, BottomArgStrictness

        let bResult, bExpr =
            match argStrictness with
            | Lazy -> defaultStrictness, Core.mapExpr (fun v -> {var=v; analysis=Lazy}) b
            | Strict n -> analyseExpr env n b
            | HyperStrict -> analyseExpr env System.Int32.MaxValue b

        {aResult with args=argsStrictness} |> andResult bResult,
        Core.App(aExpr, bExpr)
    | Core.Prim atoms -> 
        analysePrims env incomingArity atoms,
        Core.Prim atoms
    | Core.Unreachable -> maxStrictnessResult, Core.Unreachable

and analyseNonRec env incomingArity e =
    function
    | (b, rhs) ->
        let arity = manifestArity rhs
        let rhsResult, rhsExpr = analyseExpr env arity rhs
        let newEnv =
            env
            |> Map.add b
                   { arity = arity
                     strictness = rhsResult }
        let innerResult, innerExpr = analyseExpr env incomingArity e
        let analysisVar = innerResult.frees |> lookupAnalysis b
        let frees = innerResult.frees |> remove b
        { innerResult with frees = frees }, ((analysisVar, rhsExpr)), innerExpr

and analyseRec env incomingArity e bs = 
    let newEnv = getRecEnv env bs
    let innerResult, innerExpr = analyseExpr newEnv incomingArity e
    let binds = bs |> List.map(fun (v,rhs) ->
        let _, innerRhs = analyseRhs env rhs 
        (innerResult.frees |> lookupAnalysis v, innerRhs))
    let frees = innerResult.frees |> removeAll(bs|>List.map fst)
    { innerResult with frees = frees }, binds, innerExpr


and getRecEnv env (bs:Core.Bind<_,_> list) =
    let approximation =  bs |> List.map (fun b -> b, maxStrictnessValue)
    fixRec env [] approximation

and fixRec env bs approx =
    let newApprox = getNextApprox env bs approx
    if approx = newApprox then
        let newApproxMapping = (approx |> List.map (fun ((v, _), result) -> (v, result)))
        env 
            |> Map.toList
            |> List.append newApproxMapping
            |> Map.ofList
    else 
        fixRec env bs newApprox

and getNextApprox env bs = function
    |((v, rhs:Core.Expr<_,_>), approx)::xs ->
        let (newEnv:Map<_, StrictnessValue<_>>) = env |> Map.add v approx
        getNextApprox newEnv ((v,rhs)::bs) xs
    |[] ->
        analyseApprox env (bs|>List.rev)

and analyseApprox (env:Map<_, StrictnessValue<_>>) = function
    |(v,rhs)::bs ->
        let rhsValue, _ = analyseRhs env rhs
        ((v, rhs), rhsValue)::analyseApprox env bs
    | [] -> []

and analyseRhs env rhs :StrictnessValue<_> * Core.Expr<_,_> =
    let arity = manifestArity rhs
    let rhsResult, rhsExpr = analyseExpr env arity rhs
    {arity=arity; strictness=rhsResult}, rhsExpr


and analyseCase env incomingArity e b (alts, def) =
    let innerResult, innerExpr = analyseExpr env 0 e
    let defResult, defExpr = analyseExpr env incomingArity def
    let altsResult, newAlts = analyseAlts env incomingArity defResult alts
    let analysisVar = innerResult.frees |> lookupAnalysis b
    let frees = innerResult.frees |> remove b
    altsResult |> andResult {innerResult with frees=frees},
    Core.Case(innerExpr, analysisVar, (newAlts, defExpr))

and analyseAlts env incomingArity defResult =
    function
    | ((pat, vs), e) :: alts ->
        let innerResult, innerExpr = analyseExpr env incomingArity e
        let analysedVars = vs |> List.map(fun v -> innerResult.frees |> lookupAnalysis v)
        let frees = innerResult.frees |> removeAll vs
        let altsResult, newAlts = analyseAlts env incomingArity defResult alts
        { innerResult with frees = frees } |> orResult altsResult,
        ((pat, analysedVars), innerExpr)::newAlts
    | [] -> defResult, []

and analysePrims env incomingArity =
    function
    | Stg.AVar v :: atoms ->
        let innerResult = analysePrims env incomingArity atoms
        evaluate incomingArity (env |> Map.tryFind v) |> andResult innerResult
    | _ :: atoms -> analysePrims env incomingArity atoms
    | [] -> defaultStrictness


let rec analyseProgram binds constrs = function
    |(b, Core.TopExpr e)::xs -> 
        let env, program = analyseProgram ((b, e)::binds) constrs xs
        let _, innerRhs = analyseRhs env e 
        env,
        ({var=b; analysis=Lazy}, Core.TopExpr innerRhs)::program
    |(v, Core.TopConstr vs)::xs -> 
        let env, program = analyseProgram binds ((v,vs)::constrs) xs
        let analysedVars = vs |> List.map (fun v -> {var=v; analysis=Lazy})
        env,
        ({var=v; analysis=Lazy}, Core.TopConstr analysedVars)::program
    |[] -> 
        getRecEnv Map.empty binds,
        []

let analyse e = 
    let _env, program = analyseProgram [] [] e
    program