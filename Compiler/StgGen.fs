module StgGen
open Vars


let genVar =
    let i = ref (-1)
    fun () ->
        let next = !i
        i := !i - 1
        {Stg.name="gen"; Stg.info=None; Stg.unique=next}
let i = ref (1)
let wrapVar<'a> : 'a -> Stg.Var =
    fun (v) ->
        let next = !i
        i := !i + 1
        {Stg.name="anon"; Stg.info=Some (v:>obj); Stg.unique=next}

let wrapBinder ((x, (s, i)):Core.Binder<_>) = {wrapVar x with name = sprintf "%s_%i" s i }, (s, i)


type TopEnv =
    |IsLam of int
    |IsCaf
    |IsJoinPoint of int
    |IsConstr of int
    |IsExport of string * int
    |IsPrim

let forcedCallArity topEnv env f =
    match topEnv |> Map.tryFind f with
    |Some(IsLam x) -> Some x
    |Some(IsConstr x) -> Some x
    |_ -> None


let addFree topEnv  v frees =
        match topEnv |> Map.tryFind v with
        | Some (IsLam _) -> frees
        | Some (IsConstr _) -> frees
        | Some (IsCaf) -> frees
        | _ -> v :: frees


let rec withBinders env = function
    |(v, k)::xs -> withBinders (env |> Map.add k v) xs
    |[] -> env

let genLit topEnv env = function
    | Core.I32 i -> Wasm.I32Const i

let rec genExpr (topEnv:Map<Stg.Var, TopEnv>) (env:Map<Core.Name, Stg.Var>) e =
    let lf =
        match e with
        | Core.VarE (v) -> genApp topEnv env [] (Core.varE v)
        | Core.Lit lit -> Stg.lambdaForm (Stg.Prim [ Stg.ALit(genLit topEnv env lit) ])
        | Core.LamE(v:Core.Binder<Stg.Var>, e) -> genLam topEnv (withBinders env [v]) [fst v] e
        | Core.LetE(l, bs, e) -> genLet topEnv env e (l, bs)
        | Core.CaseE(e, v, alts) -> genCase topEnv (withBinders env [v]) e (fst v) alts
        | Core.App(a, b) -> genApp topEnv env [b] a
        | Core.Prim (w, es) -> genPrim topEnv env w es
        | Core.Unreachable -> genPrim topEnv env Wasm.Unreachable [ Core.Lit(Core.I32 -1) ]
    Stg.normLf lf


and genLam topEnv env vs =
    function
    | Core.LamE(v, e) -> genLam topEnv env (fst v :: vs) e
    | e ->
        let lf = genExpr topEnv env e
        let frees = lf.frees |> List.filter (fun free -> not (vs |> List.contains free))
        { lf with
              args = vs |> List.rev
              frees = frees }

and genLet topEnv env e =
    function
    | Core.NonRec, [v1, e1] -> genBindings topEnv env genNonRec [ v1 |>fst, genExpr topEnv (withBinders env [v1]) e1 ] (genExpr topEnv env e)
    | Core.Join, [j] -> genLetJoin topEnv (withBinders env [fst j]) j e
    | Core.Rec, vs -> 
        let newEnv = (withBinders env (vs |> List.map fst))    
        genBindings topEnv newEnv genRec (vs |> List.map(fun (v,e) -> fst v, genExpr topEnv newEnv e)) (genExpr topEnv newEnv e)
        
and genNonRec topEnv env (vs, expr) =
    match vs with
    |[] -> expr
    |v::vs -> Stg.Let(Stg.NonRec v, genNonRec topEnv env (vs, expr))
 
and genRec topEnv env (vs, expr) = 
    match vs with
    |[] -> expr
    |vs -> Stg.Let(Stg.Rec vs, expr)

        
and genBindings topEnv env mapExpr (lfEs:(_*Stg.LambdaForm) list) lf =
    let newStdConstrs, newLets = lfEs |> List.partition(fun (_, lfE) -> match lfE.expr with Stg.Constr(_)->true|_ -> false )
    let vs = newLets |> List.map fst

    let innerFrees =
        List.concat [
            newLets |> List.map snd |> List.collect(fun lf -> lf.frees) 
            newStdConstrs |> List.map snd |> List.collect(fun lf -> lf.frees) 
        ]            
        |> List.distinct
        |> List.filter (fun free -> not (lf.locals |> List.contains free))
        |> List.filter (fun free -> not (lf.args |> List.contains free))
        |> List.filter (fun free ->
            not
                (lf.lets
                 |> List.map fst
                 |> List.contains free))

    let stdConstrs = 
        List.concat [
            newStdConstrs |> List.map(function (v, {expr=Stg.Constr(f, vs)}) -> (v, f, vs))
            lf.stdConstrs
        ]

    let lets =
        List.concat
            [ newLets |> List.map(fun (x,y) -> x, y)
              lf.lets ]

    let frees =
        lf.frees
        |> List.append innerFrees
        |> List.filter (fun v -> not(vs |> List.contains v))

    { lf with
          stdConstrs = stdConstrs
          frees = frees
          lets = lets
          expr = mapExpr topEnv env (vs, lf.expr) }

and genLetJoin topEnv env (j, eJ) e =
    let lf = genExpr (topEnv |> Map.add (fst j) (IsJoinPoint 1)) (env ) e
    let lfJ = genExpr topEnv env eJ
    let args = lfJ.args

    let innerFrees =
        lfJ.frees
        |> List.distinct
        |> List.filter (fun free -> not (lf.locals |> List.contains free))
        |> List.filter (fun free -> not (lf.args |> List.contains free))
        |> List.filter (fun free ->
            not
                (lf.lets
                 |> List.map fst
                 |> List.contains free))

    let frees =
        lf.frees
        |> List.append innerFrees
        |> List.filter ((<>)(fst j))

    let expr =
        Stg.Let(
            Stg.Join(fst j, args, lfJ.expr),
            lf.expr
        )
    
    {
            lets = List.concat [lf.lets; lfJ.lets]
            locals = List.concat [args; lf.locals; lfJ.locals]
            args = []
            stdConstrs = []
            frees = frees
            expr = expr
    }

    





and genCase topEnv env e v alts =
    let lf = genExpr topEnv env e
    let def, otherAlts =
        match alts with
        |(Core.DefAlt, [], e)::otherAlts -> e, otherAlts
        |otherAlts -> Core.Unreachable, otherAlts
    let stgAlts, lfAlts = genAlts topEnv env def otherAlts
    let lfCombined = Stg.combineLf lf lfAlts

    let expr = Stg.Case(lf.expr, v, stgAlts)
    let locals = v :: lfCombined.locals
    let frees = lfCombined.frees |> List.filter ((<>) v)
    { lfCombined with
          locals = locals
          frees = frees
          expr = expr }

and genAlts topEnv env def =
    function
    | (Core.DataAlt v, vs, e) :: xs -> 
        let lfDef = genExpr topEnv env def
        genAAlt topEnv (withBinders env vs) lfDef lfDef.expr [] v (vs |> List.map fst) e xs
    | (Core.LitAlt l, [], e) :: xs ->
        let lfDef = genExpr topEnv env def
        genPAlt topEnv env lfDef lfDef.expr [] l e xs
    | [] ->
        let lfDef = genExpr topEnv env def
        genPAlts topEnv env lfDef lfDef.expr [] []

and genAAlt topEnv env lfAcc def aalts v vs e xs =
    let lfE = genExpr topEnv env e
    let lfCombined = Stg.combineLf lfE lfAcc
    let frees = lfCombined.frees |> List.filter (fun free -> not (vs |> List.contains free))
    let locals = lfCombined.locals |> List.append vs

    let lf =
        { lfCombined with
              frees = frees
              locals = locals }
    genAAlts topEnv env lf def
        (List.concat
            [ [ (genConstr topEnv env v, vs), lf.expr ]
              aalts ]) xs

and genAAlts topEnv env lfAcc def aalts =
    function
    | (Core.DataAlt v, vs, e) :: xs -> genAAlt topEnv env lfAcc def aalts v (vs |> List.map fst) e xs
    | [] -> Stg.AAlts(aalts, def), lfAcc

and genConstr topEnv env = function
| Core.IntDestr -> 1
| Core.Constr i -> 2 + i

and genPAlt topEnv env lfAcc def palts l e xs =
    let lfE = genExpr topEnv env e
    let lfCombined = Stg.combineLf lfE lfAcc
    genPAlts topEnv env lfCombined def
        (List.concat
            [ [ genLit topEnv env l, lfCombined.expr ]
              palts ]) xs

and genPAlts topEnv env lfAcc def (palts: Stg.PAlts) =
    function
    | (Core.LitAlt l, [], e) :: xs -> genPAlt topEnv env lfAcc def palts l e xs
    | [] -> Stg.PAlts(palts, def), lfAcc


and genApp topEnv env args =
    function
    | Core.App(f, a) -> genApp topEnv env (a :: args) f
    | Core.VarE v -> genAppWithArgs topEnv env (env |> Map.find v) args

and genAppWithArgs topEnv env f args =
    let arity = forcedCallArity topEnv env (f)
    match arity with 
    |None -> 
        genAtoms topEnv env (genAppWithAtoms topEnv env f) [] args
    |Some i when i = (args |> List.length) -> 
        // Saturated
        genAtoms topEnv env (fun xs -> Stg.lambdaForm (genCallWithAtoms topEnv f xs)) [] args
    |Some i when i < (args |> List.length) -> 
        // Oversaturated
        let firstArgs = args |> List.take i
        let secondArgs = args |> List.skip i
        let firstResult = genAppWithArgs topEnv env f firstArgs
        genAtom topEnv env (fun f -> genAppWithArgs topEnv env f secondArgs) firstResult
    |Some i when i > (args |> List.length) -> 
        // Undersaturated
        failwith "Undersaturated"
    
and genAppWithAtoms topEnv env (f:Stg.Var) atoms =
    let e = genCallWithAtoms topEnv f atoms
    let lf = Stg.lambdaForm e
    { lf with frees = lf.frees |> addFree topEnv f }

and genCallWithAtoms topEnv (f:Stg.Var) atoms =   
    match topEnv |> Map.tryFind f with
    |Some (IsJoinPoint _) -> Stg.Jump(f, atoms)
    |Some (IsLam _) -> Stg.Call(f, atoms)
    |Some (IsConstr _) -> Stg.Constr(f, atoms)
    |_ -> Stg.App(f, atoms)


and genAtoms topEnv env mapAtoms xs =
    function
    | (Core.VarE arg) :: args ->
        let (lf:Stg.LambdaForm) = genAtoms topEnv env mapAtoms (Stg.AVar (env |> Map.find arg) :: xs) args
        let frees = lf.frees |> addFree topEnv (env |> Map.find arg)
        { lf with frees = frees }
    | (Core.Lit arg) :: args -> genAtoms topEnv env mapAtoms (Stg.ALit(genLit topEnv env arg) :: xs) args
    | arg :: args ->
        genAtom topEnv env (fun var -> genAtoms topEnv env mapAtoms (Stg.AVar var :: xs) args) (genExpr topEnv env arg)
    | [] -> mapAtoms (xs |> List.rev)

and genAtom topEnv env f (arg : Stg.LambdaForm) =    
    let var = genVar()
    let lfInner = f var
    let lfE = arg
    genBindings topEnv env (genNonRec) [var, lfE] lfInner

and genPrim topEnv env w ps =
    genAtoms topEnv env (fun atoms -> Stg.lambdaForm (Stg.Prim(Stg.ALit w::atoms))) [] ps

let rec genTopLam topEnv env x args = function
    |Core.LamE(a, b) -> genTopLam topEnv (withBinders env [a]) (x-1) (fst a::args) b
    |e when x = 0 -> 
        let lf = genExpr topEnv env e
        let frees = lf.frees |> List.filter(fun x -> not(args |> List.contains x))
        if frees.Length <> 0 then failwith "Cannot have frees"
        args|>List.rev, {lf with frees=frees} 

let genTopLevel topEnv env =
    function
    | b, Core.TopExpr (_, e) -> 
        match topEnv |> Map.tryFind (fst b) with
        |Some IsCaf -> fst b, Stg.TopCaf(genExpr topEnv env e)
        |Some (IsExport (name, arity)) -> 
            let args, e = genTopLam topEnv env arity [] e
            fst b, Stg.TopExport(name, args, e)
        |Some (IsLam arity) -> fst b, Stg.TopLam(genTopLam topEnv env arity [] e)
    | b, Core.TopConstr(c, vs) -> fst b, Stg.TopConstr (genConstr topEnv env c, vs |> List.map snd)

let rec getManifestArity = function
    |Core.LamE(a, b) -> 1 + getManifestArity b
    |_ -> 0




let rec uniqueifyExpr = function
    | Core.VarE(v) -> Core.varE (v)
    | Core.Lit(l) -> Core.Lit(l)
    | Core.LamE(x, e) -> Core.lamE(wrapBinder x, uniqueifyExpr e) 
    | Core.LetE(l, bs, e) ->  Core.letE(l, uniqueifyBinds  bs, uniqueifyExpr e)
    | Core.CaseE(e, b, alts) ->  Core.caseE(uniqueifyExpr e, wrapBinder b, uniqueifyAlts alts)
    | Core.App(a, b) -> Core.App(uniqueifyExpr a, uniqueifyExpr b)
    | Core.Prim(p, es) -> Core.Prim(p, es |> List.map (uniqueifyExpr))
    | Core.Unreachable -> Core.Unreachable

and uniqueifyAlts alts =
    alts |> List.map(fun (a,bs,e)->a, bs |> List.map wrapBinder, uniqueifyExpr e)

and uniqueifyBinds = function
    |(b, e)::xs -> (wrapBinder b, uniqueifyExpr e)::uniqueifyBinds xs
    |[] -> []
    
and uniquifyProgram program =
     program |> List.map(function
        | b, Core.TopConstr (c, vs) -> wrapBinder b, Core.TopConstr (c, vs |> List.map (fun (s, x) -> (s, wrapVar x)))
        | b, Core.TopExpr (export, e) -> wrapBinder b, Core.TopExpr (export, uniqueifyExpr e)
    ) 

let genProgram (Core.Program core): Stg.Program =
    let unique = uniquifyProgram core
    let topEnv = unique |> List.map(function
        | b, Core.TopConstr (c, vs) -> fst b, IsConstr (vs.Length)
        | b, Core.TopExpr (Core.NoExport, e) when getManifestArity e > 0 -> fst b, IsLam (getManifestArity e)
        | b, Core.TopExpr (Core.NoExport, e) -> fst b, IsCaf
        | b, Core.TopExpr (Core.Export s, e) -> fst b, IsExport (s, getManifestArity e)
    ) 
    let program = unique |> List.map (genTopLevel (topEnv |> Map.ofList) (withBinders  Map.empty (unique |> List.map fst)))
    program
