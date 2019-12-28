module CoreGen

type GuardLHS = Ast.Var
type GuardRHS = Ast.Pattern<Ast.Var>
type GuardedCases = GuardLHS list * (GuardRHS list * Core.Expr<Ast.Var>) list

type Default =
    |NoDefault
    |DefaultExpr of Core.Expr<Ast.Var>
    |DefaultVar of Ast.Var


let rec genExpr = function
    | Ast.Lit l -> genLit l
    | Ast.Var v -> Core.Var v
    | Ast.Call (f, args) -> genCall (Core.Var f) args
    | Ast.Match (e, cases) -> genMatch e cases
    | Ast.Block (args, block) -> genBlock None [] args block
    | Ast.Prim ps -> genPrim [] ps

and genLit = function
    | Ast.Box(Ast.Integer i) -> 
        Core.App(Core.Var(Ast.builtInVar Ast.IntegerConstr), Core.Lit (Core.I32 i))
    | _ -> failwith "TODO"

and genCall f = function
    |arg::args ->
        genCall (Core.App(f, genExpr arg)) args
    |[] -> f

and genMatch e cases =    
    let manyCases = (cases |> List.map(fun (pat, expr) -> ([pat], expr)))
    genManyMatch NoDefault [e] manyCases

and genManyMatch def es (cases:(Ast.Pattern<Ast.Var> list * Ast.Expr<Ast.Var>) list) =
    match es with
    |matchexpr::es ->
        genCases ([], matchexpr, es, NoDefault, None) cases
    |[] ->
        match cases with
        |[[], e] -> genExpr e
        |_ -> failwith "Error"


and genCases (alts, matchexpr, es, def, bind) cases =
    let state = (alts, matchexpr, es, def, bind) 
    match cases with
    | ((Ast.PatLit(Ast.Raw l)::_, _))::xs->
        genPatLit state l [] [] cases
    | ((Ast.PatLit(Ast.Box (Ast.Integer _))::_, _))::xs->
        genPatConstr state (Ast.builtInVar Ast.IntegerConstr) [Ast.freshVar()] [] [] cases
    | [[Ast.PatConstr(v, [Ast.PatBind v1])], e] ->
        genPatConstr state v [v1] [] [] cases
    | [[Ast.PatBind(bindV)], e] ->
        match def with
        |NoDefault ->
            genCases (alts, matchexpr, es, DefaultExpr (genExpr e), Some bindV) []
        |_ ->
            genCases (alts, matchexpr, es, def, Some bindV) []
    |[] -> 
        let bindV =
            match bind with
            |Some bindV -> bindV
            |None -> Ast.freshVar()
        match def with
        |NoDefault ->
            let defE = Core.Prim[Stg.ALit Wasm.Unreachable]
            let defV = Ast.freshVar()
            Core.Let(Core.NonRec[defV, defE],
                Core.Case(genExpr matchexpr, bindV, (alts, Core.Var(defV))))
        |DefaultExpr defE ->
            let defV = Ast.freshVar()
            Core.Let(Core.NonRec[defV, defE],
                Core.Case(genExpr matchexpr, bindV, (alts, Core.Var(defV))))
    |other -> failwithf "TODO"

and genPatConstr state v (vs:Ast.Var list) (subCases:(Ast.Pattern<Ast.Var> list * Ast.Expr<Ast.Var>) list) otherCases = function
    | ((Ast.PatLit(Ast.Box (Ast.Integer i))::cases, e))::xs when v = Ast.builtInVar Ast.IntegerConstr ->
        let subCase = (Ast.PatLit(Ast.Raw(Core.I32 i)))::cases, e
        genPatConstr state v vs (subCase::subCases) otherCases xs
    | ((Ast.PatConstr(v1, vs1)::cases, e))::xs when v = v1 ->
        genPatConstr state v vs ((List.concat [vs1; cases], e)::subCases) otherCases xs
    |x::xs ->
        genPatConstr state v vs subCases (x::otherCases) xs
    | [] ->
        let (alts, matchexpr, es, def, bind) = state
        let vsExprs = vs |> List.map (Ast.Var)
        let subEs = List.concat [vsExprs; es]
        let e = genManyMatch def subEs subCases
        let alt = ((Core.Var v, vs), e)
        genCases (alt::alts, matchexpr, es, def, bind) otherCases

and genPatLit state (l:Core.Lit) (subCases:(Ast.Pattern<Ast.Var> list * Ast.Expr<Ast.Var>) list) otherCases = function
    | ((Ast.PatLit(Ast.Raw l1)::cases, e))::xs when l = l1 ->
        genPatLit state l ((cases, e)::subCases) otherCases xs
    |x::xs ->
        genPatLit state l subCases (x::otherCases) xs
    | [] ->
        let (alts, matchexpr, es, def, bind) = state
        let e = genManyMatch def es subCases
        let alt = ((Core.Lit l, []), e)
        genCases (alt::alts, matchexpr, es, def, bind) otherCases


    




    

and genBlock returnValue lets args = function
    |Ast.Assign(lhs, rhs)::xs ->
        genBlock returnValue ((lhs, genExpr rhs)::lets) args xs
    |Ast.Return(e)::xs ->
        match returnValue with
        |None -> genBlock (Some (genExpr e)) lets args xs
        |Some _ -> failwith "Multiple return values"
    |[] ->
        match returnValue with
        |None -> failwith "No return value of block"
        |Some value -> Core.Let(Core.NonRec lets, value)

and genPrim xs = function
    |Ast.PrimVar v::ps ->
        genPrim (Stg.AVar v::xs) ps
    |Ast.PrimWasm w::ps ->
        genPrim (Stg.ALit w::xs) ps
    |[] ->
        Core.Prim (xs |> List.rev)


let genDeclaration = function
    |Ast.GlobalDecl(v, e) ->
        v, Core.TopExpr(genExpr e)
    |Ast.TypeDecl(v, vs) ->
        v, Core.TopConstr(vs)

let genProgram (ast:Ast.Program<_>) : Core.Program<_> = 
    ast |>
        List.map genDeclaration