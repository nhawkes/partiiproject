module Analysis

open Vars

type Path = int list
type Args = int
type Strictness =
    |Lazy
    |Strict of Args
    |HyperStrict

type ArgStrictness =
    |BottomArgStrictness
    |ArgsStrictness of Strictness * ArgStrictness
    |TopArgStrictness

type DefaultMap<'k, 'v> when 'k:comparison = 'v * Map<'k, 'v>
type StrictnessResult = {
    args: ArgStrictness
    frees: DefaultMap<Var, Strictness>
}
type StrictnessValue = {
    arity: int
    strictness: StrictnessResult
}


let lookup k (d, map) =
    match map |> Map.tryFind k with
    |None -> d
    |Some v -> v 

let update k v (d, map) = (d, map |> Map.add k v)
let remove k (d, map) = (d, map |> Map.remove k)

let orStrictness = function
    |HyperStrict, x -> x
    |x, HyperStrict -> x
    |Strict(n1), Strict(n2) -> Strict(min n1 n2)
    |Lazy, _ -> Lazy
    |_, Lazy -> Lazy

let andStrictness = function
    |Lazy, x -> x
    |x, Lazy -> x
    |Strict(n1), Strict(n2) -> Strict(max n1 n2)
    |HyperStrict, _ -> HyperStrict
    |_, HyperStrict -> HyperStrict
    
let rec orArgs = function
    |BottomArgStrictness, x -> x
    |x, BottomArgStrictness -> x
    |ArgsStrictness(x1, y1), ArgsStrictness(x2, y2) ->
        ArgsStrictness(orStrictness (x1, x2), orArgs (y1, y2))
    |TopArgStrictness, _ -> TopArgStrictness
    |_, TopArgStrictness -> TopArgStrictness

let rec andArgs = function
    |TopArgStrictness, x -> x
    |x, TopArgStrictness -> x
    |ArgsStrictness(x1, y1), ArgsStrictness(x2, y2) ->
        ArgsStrictness(orStrictness (x1, x2), orArgs (y1, y2))
    |BottomArgStrictness, _ -> BottomArgStrictness
    |_, BottomArgStrictness -> BottomArgStrictness

let combineFrees combine ((d1, map1), (d2, map2))= 
    let getKeys = Map.toList >> List.map fst
    let keys = List.concat [getKeys map1; getKeys map2] |> List.distinct
    let map = 
        keys |>
        List.map(fun key ->
            key,
            combine((d1, map1) |> lookup key, (d2, map2) |> lookup key)
        ) |>
        Map.ofList
    orStrictness(d1, d2), map
let orFrees = combineFrees orStrictness 
let andFrees = combineFrees andStrictness    

let orResult a b =
    {
        args=orArgs(a.args, b.args)
        frees=orFrees(a.frees, b.frees)
    }
let andResult a b =
    {
        args=andArgs(a.args, b.args)
        frees=andFrees(a.frees, b.frees)
    }



let defaultStrictness =
    {
        args=TopArgStrictness
        frees=(Lazy, Map.empty)
    }

let evaluate incomingArity {arity=arity; strictness=strictness} =
    if incomingArity>arity then
        strictness
    else
        defaultStrictness

let rec manifestArity = function
    |Core.Lam(_, e) -> 1 + manifestArity e
    |_ -> 0

let rec analyseExpr env incomingArity = function
    | Core.Var v -> 
        evaluate incomingArity (env |> lookup v)
    | Core.Lit l ->
        defaultStrictness
    | Core.Lam(x, e) ->
        let innerResult =
            match incomingArity with
            |0 -> defaultStrictness
            |_ ->
                analyseExpr env (incomingArity-1) e 
        let argStrictness = innerResult.frees |> lookup x
        let frees = innerResult.frees |> remove x
        let args = ArgsStrictness(argStrictness, innerResult.args)
        {args = args; frees = frees}
    | Core.Let(bs, e) -> 
        match bs with
        |Core.NonRec bs -> analyseNonRec env incomingArity e bs
    | Core.Case(e, v, alts) -> analyseCase env incomingArity e v alts
    | Core.App(a, b) -> 
        let aResult = analyseExpr env (incomingArity+1) a
        let argStrictness, argsStrictness =
            match aResult.args with
            |TopArgStrictness -> Lazy, TopArgStrictness
            |ArgsStrictness(x, xs ) -> x, xs
            |BottomArgStrictness -> Strict, BottomArgStrictness
        let bResult =
            match argStrictness with
            |Lazy -> defaultStrictness
            |Strict n -> analyseExpr env n b
            |HyperStrict -> analyseExpr env System.Int32.MaxValue b
        aResult |> andResult bResult
    | Core.Prim atoms ->
        analysePrims env incomingArity atoms

and analyseNonRec env incomingArity e = function
    |(b, rhs)::bs -> 
        let arity = manifestArity rhs
        let rhsResult = analyseExpr env arity rhs
        let newEnv = env |> update b {arity=arity; strictness=rhsResult}
        let innerResult = analyseNonRec newEnv incomingArity e bs
        let frees = innerResult.frees |> remove b
        {innerResult with frees=frees}
    |[] -> analyseExpr env incomingArity e

and analyseCase env incomingArity e v (alts, def) = 
    let innerResult = analyseExpr env 0 e
    let defResult = analyseExpr env incomingArity def
    analyseAlts env incomingArity defResult alts |> andResult innerResult

and analyseAlts env incomingArity defResult = function
    |((pat, vs), e)::alts ->
        let innerResult = analyseExpr env incomingArity e
        let frees = vs |> List.fold (fun frees v -> frees |> remove v) innerResult.frees 
        let altsResult = analyseAlts env incomingArity defResult alts
        {innerResult with frees=frees} |> orResult altsResult
    |[] -> defResult

let rec analysePrims env incomingArity = function
    |Stg.AVar v::atoms -> 
        let innerResult = analysePrims env incomingArity atoms
        evaluate incomingArity (env |> lookup v) |> andResult innerResult
    |_::atoms -> analysePrims env incomingArity atoms
    |[] -> defaultStrictness
