module Analysis

open Vars

type Path = int list

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


let lookup k (d, map) =
    match map |> Map.tryFind k with
    | None -> d
    | Some v -> v

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
    let def = orStrictness (d1, d2)
    let map =
        keys
        |> List.map (fun key -> key, combine ((d1, map1) |> lookup key, (d2, map2) |> lookup key))
        |> List.filter(fun (_, value) -> value=def)
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
    | Some { arity = arity; strictness = strictness } when incomingArity > arity -> strictness
    | _ -> defaultStrictness

let rec manifestArity =
    function
    | Core.Lam(_, e) -> 1 + manifestArity e
    | _ -> 0

let rec analyseExpr (env: Map<_, _>) incomingArity =
    function
    | Core.Var v -> 
        let innerResult = evaluate incomingArity (env |> Map.tryFind v)
        let unitResult = {
            frees=(Lazy, Map.ofList[v, Strict incomingArity])
            args=TopArgStrictness
        } 
        unitResult |> andResult innerResult        
    | Core.Lit l -> defaultStrictness
    | Core.Lam(x, e) ->
        let innerResult =
            match incomingArity with
            | 0 -> defaultStrictness
            | _ -> analyseExpr env (incomingArity - 1) e

        let argStrictness = innerResult.frees |> lookup x
        let frees = innerResult.frees |> remove x
        let args = ArgsStrictness(argStrictness, innerResult.args)
        { args = args
          frees = frees }
    | Core.Let(bs, e) ->
        match bs with
        | Core.NonRec bs -> analyseNonRec env incomingArity e bs
        | Core.Rec bs -> analyseRec env incomingArity e bs
    | Core.Case(e, v, alts) -> analyseCase env incomingArity e v alts
    | Core.App(a, b) ->
        let aResult = analyseExpr env (incomingArity + 1) a

        let argStrictness, argsStrictness =
            match aResult.args with
            | TopArgStrictness -> Lazy, TopArgStrictness
            | ArgsStrictness(x, xs) -> x, xs
            | BottomArgStrictness -> HyperStrict, BottomArgStrictness

        let bResult =
            match argStrictness with
            | Lazy -> defaultStrictness
            | Strict n -> analyseExpr env n b
            | HyperStrict -> analyseExpr env System.Int32.MaxValue b

        aResult |> andResult bResult
    | Core.Prim atoms -> analysePrims env incomingArity atoms

and analyseNonRec env incomingArity e =
    function
    | (b, rhs) :: bs ->
        let arity = manifestArity rhs
        let rhsResult = analyseExpr env arity rhs

        let newEnv =
            env
            |> Map.add b
                   { arity = arity
                     strictness = rhsResult }
        let innerResult = analyseNonRec newEnv incomingArity e bs
        let frees = innerResult.frees |> remove b
        { innerResult with frees = frees }
    | [] -> analyseExpr env incomingArity e

and analyseRec env incomingArity e bs = 
    let newEnv = getRecEnv env bs
    let innerResult = analyseExpr newEnv incomingArity e
    let frees = innerResult.frees |> removeAll(bs|>List.map fst)
    { innerResult with frees = frees }


and getRecEnv env bs =
    let approximation =  bs |> List.map (fun b -> b, maxStrictnessValue)
    fixRec env [] approximation

and fixRec env bs approx =
    let newApprox = getNextApprox env bs approx
    if approx = newApprox then
        env 
            |> Map.toList
            |> List.append (approx |> List.map (fun ((v, _), result) -> (v, result)))
            |> Map.ofList
    else
        fixRec env bs newApprox

and getNextApprox env bs = function
    |((v, rhs:Core.Expr<_,_>), approx)::xs ->
        let (newEnv:Map<'a, StrictnessValue<'a>>) = env |> Map.add v approx
        getNextApprox newEnv ((v,rhs)::bs) xs
    |[] ->
        analyseApprox env bs

and analyseApprox (env:Map<'a, StrictnessValue<'a>>) = function
    |(v,rhs)::bs ->
        let arity = manifestArity rhs
        let rhsResult = analyseExpr env arity rhs
        ((v, rhs), {arity=arity; strictness=rhsResult})::analyseApprox env bs
    | [] -> []


    
    
    

and analyseCase env incomingArity e v (alts, def) =
    let innerResult = analyseExpr env 0 e
    let defResult = analyseExpr env incomingArity def
    analyseAlts env incomingArity defResult alts |> andResult innerResult

and analyseAlts env incomingArity defResult =
    function
    | ((pat, vs), e) :: alts ->
        let innerResult = analyseExpr env incomingArity e
        let frees = vs |> List.fold (fun frees v -> frees |> remove v) innerResult.frees
        let altsResult = analyseAlts env incomingArity defResult alts
        { innerResult with frees = frees } |> orResult altsResult
    | [] -> defResult

and analysePrims env incomingArity =
    function
    | Stg.AVar v :: atoms ->
        let innerResult = analysePrims env incomingArity atoms
        evaluate incomingArity (env |> Map.tryFind v) |> andResult innerResult
    | _ :: atoms -> analysePrims env incomingArity atoms
    | [] -> defaultStrictness

let analyse e = analyseExpr Map.empty 0 e
