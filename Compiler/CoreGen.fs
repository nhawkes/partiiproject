module CoreGen

type GuardLHS = Ast.Var

type GuardRHS = Ast.Pattern<Ast.Var>

type GuardedCases = GuardLHS list * (GuardRHS list * Core.Expr<Ast.Var>) list

type Default =
    | NoDefault
    | DefaultExpr of Core.Expr<Ast.Var>
    | DefaultVar of Ast.Var


let rec genExpr =
    function
    | Ast.Lit l -> genLit l
    | Ast.Var v -> Core.Var v
    | Ast.Call(f, args) -> genCall (Core.Var f) args
    | Ast.Match(e, cases) -> genMatch e cases
    | Ast.Block( block) -> genBlock None [] block
    | Ast.Prim ps -> genPrim [] ps

and genLit =
    function
    | Ast.Box(Ast.Integer i) -> Core.App(Core.Var(Ast.builtInVar Ast.IntegerConstr), Core.Lit(Core.I32 i))
    | _ -> failwith "TODO"

and genCall f =
    function
    | arg :: args -> genCall (Core.App(f, genExpr arg)) args
    | [] -> f

and genMatch e cases =
    let manyCases = (cases |> List.map (fun (pat, expr) -> ([ pat ], expr)))
    genManyMatch (Core.Prim[Stg.ALit Wasm.Unreachable]) [ e ] manyCases

and genManyMatch def es (cases: (Ast.Pattern<Ast.Var> list * Ast.Expr<Ast.Var>) list) =
    match es with
    | (matchexpr, typ) :: es -> genCases (def, (matchexpr, typ), es, None) [] [] cases
    | [] ->
        match cases with
        | [ [], e ] -> genExpr e
        | [] -> def


and genCases (def, (matchexpr, typ), es, bind) subCases otherCases = function
    |(Ast.PatBind(bindV)::xs, e)::cases -> 
        genCases (def, (matchexpr, typ), es, Some bindV) ((xs, e)::subCases) otherCases cases
    |case::cases -> 
        genCases (def, (matchexpr, typ), es, bind) (subCases) (case::otherCases) cases
    |[] ->
        let def =
            genManyMatch def es subCases
        genCasesWithDefault (def, [], (matchexpr, typ), es, bind |> Option.defaultWith(fun () -> Ast.freshVar Ast.ValueT)) otherCases 
    


and genCasesWithDefault (def, alts, (matchexpr, typ), es, bind) cases =
    let state = (alts, (matchexpr, typ), es, def, bind)
    match cases with
    | ((Ast.PatLit(Ast.Raw l) :: _, _)) :: xs -> genPatLit state l [] [] cases
    | ((Ast.PatLit(Ast.Box(Ast.Integer _)) :: _, _)) :: xs ->
        genPatConstr state (Ast.builtInVar Ast.IntegerConstr) [ Ast.freshVar Ast.IntT ] [] [] cases
    | ([ Ast.PatConstr(v, [ Ast.PatBind v1 ]) ], e) :: xs -> genPatConstr state v [ v1 ] [] [] cases
    | (((Ast.PatBind(_)::_), e)) :: xs -> 
        genCasesWithDefault (def, alts, (matchexpr, typ), es, bind) xs
    | [] ->
        Core.Case(genExpr matchexpr, bind, (alts, def))

and genPatConstr state v (vs: Ast.Var list) (subCases: (Ast.Pattern<Ast.Var> list * Ast.Expr<Ast.Var>) list) otherCases =
    function
    | ((Ast.PatLit(Ast.Box(Ast.Integer i)) :: cases, e)) :: xs when v = Ast.builtInVar Ast.IntegerConstr ->
        let subCase = (Ast.PatLit(Ast.Raw(Core.I32 i))) :: cases, e
        genPatConstr state v vs (subCase :: subCases) otherCases xs
    | ((Ast.PatConstr(v1, vs1) :: cases, e)) :: xs when v = v1 ->
        genPatConstr state v vs ((List.concat [ vs1; cases ], e) :: subCases) otherCases xs
    | x :: xs -> genPatConstr state v vs subCases (x :: otherCases) xs
    | [] ->
        let (alts, (matchexpr, typ), es, def, bind) = state
        let vsExprs = vs |> List.map (fun v -> (Ast.Var v, v.typ))
        let subEs = List.concat [ vsExprs; es ]
        let e = genManyMatch def subEs subCases
        let alt = ((Core.Var v, vs), e)
        genCasesWithDefault (def, alt :: alts, (matchexpr, typ), es, bind) otherCases

and genPatLit state (l: Core.Lit) (subCases: (Ast.Pattern<Ast.Var> list * Ast.Expr<Ast.Var>) list) otherCases =
    function
    | ((Ast.PatLit(Ast.Raw l1) :: cases, e)) :: xs when l = l1 ->
        genPatLit state l ((cases, e) :: subCases) otherCases xs
    | x :: xs -> genPatLit state l subCases (x :: otherCases) xs
    | [] ->
        let (alts, (matchexpr, typ), es, def, bind) = state
        let e = genManyMatch def es subCases
        let alt = ((Core.Lit l, []), e)
        genCasesWithDefault (def, alt :: alts, (matchexpr, typ), es, bind) otherCases

and genBlock returnValue lets =
    function
    | Ast.Assign(lhs, args, rhs) :: xs -> 
        genBlock returnValue ((lhs, genRhs (genExpr rhs) args) :: lets) xs
    | Ast.Return(e) :: xs ->
        match returnValue with
        | None -> genBlock (Some(genExpr e)) lets xs
        | Some _ -> failwith "Multiple return values"
    | [] ->
        match returnValue with
            | None -> failwith "No return value of block"
            | Some value -> Core.Let(Core.NonRec lets, value)
        

and genRhs (rhs:Core.Expr<_>) = function
    | x :: xs -> Core.Lam(x, genRhs rhs xs)
    | [] -> rhs
            

and genPrim xs =
    function
    | Ast.PrimVar v :: ps -> genPrim (Stg.AVar v :: xs) ps
    | Ast.PrimWasm w :: ps -> genPrim (Stg.ALit w :: xs) ps
    | [] -> Core.Prim(xs |> List.rev)

let rec genExport call args rhs =
    match args with
    |x::xs ->
        genExport (Core.App(call, Core.App(Core.Var (Ast.builtInVar Ast.IntegerConstr), Core.Var x))) xs rhs
    |[] -> 
        let resultVar = Ast.freshVar Ast.ValueT
        let returnVar = Ast.freshVar Ast.IntT
        Core.Case(
            call,                        
            resultVar,
            Core.Alts(       
                [(Core.Var (Ast.builtInVar Ast.IntegerConstr), [returnVar]),
                    Core.Var(returnVar)            
                ],
                Core.Prim [Stg.ALit Wasm.Unreachable]
            )
        )   

let genDeclaration =
    function
    | Ast.GlobalDecl(lhs : Ast.Var, args, rhs) ->
        [lhs, Core.TopExpr(genRhs (genExpr rhs) args)]
    | Ast.ExportDecl(name : string, args, rhs) ->
        let globalTyp = Ast.TopFuncT(List.replicate (args |> List.length) Ast.ValueT, Ast.ValueT)
        let exportTyp = Ast.TopFuncT(List.replicate (args |> List.length) Ast.IntT, Ast.IntT)
        let globalVar = Ast.globalVar name globalTyp
        let exportVar = Ast.exportVar name exportTyp
        let globalArgs = args |> List.map (fun arg -> Ast.localVar arg Ast.ValueT)
        let exportArgs = args |> List.map (fun arg -> Ast.localVar arg Ast.IntT)
        [
            globalVar, Core.TopExpr(genRhs (genExpr rhs) globalArgs)
            exportVar, Core.TopExpr(
                genRhs (genExport (Core.Var globalVar) exportArgs rhs) exportArgs)
        ]
    | Ast.TypeDecl(v, vs) -> [v, Core.TopConstr(vs)]

let genProgram (ast: Ast.Program<_>): Core.Program<_> = ast |> List.collect genDeclaration