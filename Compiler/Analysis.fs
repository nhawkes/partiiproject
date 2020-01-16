module Analysis

open Vars

type Path = int list
type Predicate =
    |IsUsedMany
    |IsUsedExactlyOnce
    |IsNeverUsed
type Predicates = Map<Var, Predicate>
type Effect = 
    |Top
    |Predicates of Predicates
    |AfterCallHasEffect of Var list * Effect
    |Bottom
type EvaluationEffects = Map<Var, Effect>

let lubCombineFunc xs ys =
    failwith "TODO"    

let lubCombineValue x y =
    failwith "TODO"   

let lubCombine = function
    |Top, _ -> Top
    |_, Top -> Top
    |FuncAnnotation xs, FuncAnnotation ys -> lubCombineFunc xs ys
    |ValueAnnotation x, ValueAnnotation y -> lubCombineValue x y
    |Bottom, x -> x
    |x, Bottom -> x

let lubCombineEnv env1 env2 =
    let keys1 = env1 |> Map.toList |> List.map fst
    let keys2 = env2 |> Map.toList |> List.map fst
    let keys = List.append keys1 keys2 |> List.distinct
    keys 
        |> List.map(fun k -> 
            (k, lubCombine (env1 |> Map.tryFind k |> Option.defaultValue Bottom, env2 |> Map.tryFind k |> Option.defaultValue Bottom)))
        |> Map.ofList

let seqCombineFunc xs ys =
    failwith "TODO"    

let seqCombineValue x y =
    failwith "TODO"   

let seqCombine = function
    |IsUsedMany, _ -> IsUsedMany
    |_, IsUsedMany -> IsUsedMany
    |IsUsedExactlyOnce, IsUsedExactlyOnce -> IsUsedMany
    |IsNeverUsed, x -> x
    |x, IsNeverUsed -> x

let seqCombineEnv env1 env2 =
    let keys1 = env1 |> Map.toList |> List.map fst
    let keys2 = env2 |> Map.toList |> List.map fst
    let keys = List.append keys1 keys2 |> List.distinct
    keys 
        |> List.map(fun k -> 
            (k, seqCombine (env1 |> Map.tryFind k |> Option.defaultValue IsNeverUsed, env2 |> Map.tryFind k |> Option.defaultValue IsNeverUsed)))
        |> Map.ofList    
      
let removeFrees frees env =
    env |> Map.filter(fun k v -> frees |> List.contains k)

// expr is evaluated => predicates
let rec analyseExprIsEvaluated = function
    | Core.Var v -> analyseVarIsEvaluated v
    | Core.Lit lit -> analyseLitIsEvaluated lit
    | Core.Lam(v, e) -> analyseLamIsEvaluated v e
    | Core.Let(bs, e) -> analyseLetIsEvaluated bs e
    | Core.Case(e, v, alts) -> analyseCase e v alts
    | Core.App(a, b) -> analyseApp a b
    | Core.Prim ps -> analysePrim ps

and analyseVarIsEvaluated v =
    Map.ofList [v, IsUsedExactlyOnce]


and analyseLitIsEvaluated lit = 
    Map.empty

and analyseLamIsEvaluated v e = 
    let env = analyseExprIsEvaluated e
    env |> Map.map(fun k v -> IsUsedMany)


and analyseLetIsEvaluated bs e = 
    let env = analyseExprIsEvaluated e
    match bs with
    |Core.NonRec xs -> xs |> List.fold analyseNonRecWithEnv env

and analyseNonRecWithEnv env ((v, e):Core.Bind<_>)  =
    match env |> Map.tryFind v |> Option.defaultValue IsNeverUsed with
    |IsUsedMany ->
        let innerEnv = analyseExprIsEvaluated e
        seqCombineEnv env innerEnv |>  removeFrees [v]       
    |IsUsedExactlyOnce ->
        let innerEnv = analyseExprIsEvaluated e
        seqCombineEnv env innerEnv |>  removeFrees [v]  
    |IsNeverUsed -> env

and analyseCaseIsEvaluated e v alts = 
    let env = analyseExprIsEvaluated e
    

and analyseAppIsEvaluated a b =
    0

and analysePrimIsEvaluated ps =
    0