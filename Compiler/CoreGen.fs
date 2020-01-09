module CoreGen

type GuardLHS = Vars.Var

type GuardRHS = Ast.Pattern<Vars.Var>

type GuardedCases = GuardLHS list * (GuardRHS list * Core.Expr<Vars.Var>) list

type Default =
    | NoDefault
    | DefaultExpr of Core.Expr<Vars.Var>
    | DefaultVar of Vars.Var


let rec genExpr =
    function
    | Ast.Lit l -> genLit l
    | Ast.Var v -> Core.Var v
    | Ast.Call(f, args) -> genCall (Core.Var f) args
    | Ast.BinOp(x, f, y) -> genBinOp x y f
    | Ast.Match(e, cases) -> genMatch e cases
    | Ast.Block( block) -> genBlock None [] block
    | Ast.Prim ps -> genPrim [] ps

and genLit =
    function
    | Ast.Box(Ast.Integer i) -> Core.App(Core.Var(Vars.integerConstr), Core.Lit(Core.I32 i))
    | Ast.Raw(l) -> Core.Lit(l)

and genCall f =
    function
    | arg :: args -> genCall (Core.App(f, genExpr arg)) args
    | [] -> f

and genBinOp x y = function
    |Ast.Add -> genCall (Core.Var Vars.addOp) [x;y]
    |Ast.Sub -> genCall (Core.Var Vars.subOp) [x;y]


and genMatch e cases =
    let manyCases = (cases |> List.map (fun (pat, expr) -> ([ pat ], expr)))
    genManyMatch (Core.Prim[Stg.ALit Wasm.Unreachable; Stg.ALit (Wasm.I32Const -1)]) [ e ] manyCases

and genManyMatch def es (cases: (Ast.Pattern<Vars.Var> list * Ast.Expr<Vars.Var>) list) =
    match es with
    | (matchexpr, typ) :: es -> genCases (def, (matchexpr, typ), es, None) [] [] cases
    | [] ->
        match cases with
        | ([], e)::_ -> genExpr e
        | [] -> def


and genCases (def, (matchexpr, typ), es, bind) subCases otherCases = function
    |(Ast.PatBind(bindV)::xs, e)::cases -> 
        genCases (def, (matchexpr, typ), es, Some bindV) ((xs, e)::subCases) otherCases cases
    |case::cases -> 
        genCases (def, (matchexpr, typ), es, bind) (subCases) (case::otherCases) cases
    |[] ->
        let def =
            genManyMatch def es subCases
        genCasesWithDefault (def, [], (matchexpr, typ), es, bind |> Option.defaultWith(fun () -> Vars.generateVar Types.ValueT)) otherCases 
    


and genCasesWithDefault (def, alts, (matchexpr, typ), es, bind) cases =
    let state = (alts, (matchexpr, typ), es, def, bind)
    match cases with
    | ((Ast.PatLit(Ast.Raw l) :: _, _)) :: xs -> genPatLit state l [] [] cases
    | ((Ast.PatLit(Ast.Box(Ast.Integer _)) :: _, _)) :: xs ->
        genPatConstr state (Vars.integerConstr) [ Vars.generateVar Types.IntT ] [] [] cases
    | ([ Ast.PatConstr(v, ps) ], e) :: xs ->  
        let vs = ps |> List.map (function |Ast.PatBind(v) -> v |_->Vars.generateVar Types.ValueT)
        genPatConstr state v vs [] [] cases
    | (((Ast.PatBind(_)::_), e)) :: xs -> 
        genCasesWithDefault (def, alts, (matchexpr, typ), es, bind) xs
    | [] ->
        Core.Case(genExpr matchexpr, bind, (alts, def))

and genPatConstr state v (vs: Vars.Var list) (subCases: (Ast.Pattern<Vars.Var> list * Ast.Expr<Vars.Var>) list) otherCases =
    function
    | ((Ast.PatLit(Ast.Box(Ast.Integer i)) :: cases, e)) :: xs when v = Vars.integerConstr ->
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

and genPatLit state (l: Core.Lit) (subCases: (Ast.Pattern<Vars.Var> list * Ast.Expr<Vars.Var>) list) otherCases =
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
            | Some value -> Core.Let(Core.Rec lets, value)
        

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
        genExport (Core.App(call, Core.App(Core.Var (Vars.integerConstr), Core.Var x))) xs rhs
    |[] -> 
        let resultVar = Vars.generateVar Types.ValueT
        let returnVar = Vars.generateVar Types.IntT
        Core.Case(
            call,                        
            resultVar,
            Core.Alts(       
                [(Core.Var (Vars.integerConstr), [returnVar]),
                    Core.Var(returnVar)            
                ],
                Core.Prim [Stg.ALit Wasm.Unreachable; Stg.ALit (Wasm.I32Const -1)]
            )
        )   

let genDeclaration =
    function
    | Ast.GlobalDecl(lhs : Vars.Var, args, rhs) ->
        [lhs, Core.TopExpr(genRhs (genExpr rhs) args)]
    | Ast.ExportDecl((exportName, exportArgs), (lhs, args), rhs) ->
        let exportTyp = Types.createFuncT Types.ExportFunc (List.replicate (args |> List.length) Types.IntT) (Types.IntT)
        let exportVar = Vars.exportVar exportName exportTyp
        let exportArgs = exportArgs |> List.map (fun arg -> Vars.userVar arg Types.IntT)
        [
            lhs, Core.TopExpr(genRhs (genExpr rhs) args)
            exportVar, Core.TopExpr(
                genRhs (genExport (Core.Var lhs) exportArgs rhs) exportArgs)
        ]
    | Ast.TypeDecl(v, vs) -> [v, Core.TopConstr(vs)]

let genProgram (ast: Ast.Program<_>): Core.Program<_> = ast |> List.collect genDeclaration
