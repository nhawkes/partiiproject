module CoreGen

open Vars

type MatchExpr =
    |MatchExpr of Ast.Expr
    |MatchVar of Var    

let genVar =
    let i = ref 0
    fun typ ->
        let next = !i
        i := !i + 1
        Gen typ, next
        



let genProgram (ast: Ast.Program): Core.ClosedProgram<_> = 
    let constr = ast |> List.choose(function 
        | Ast.TypeDecl (Ast.AssignVar v) -> Some v
        | Ast.TypeDecl (Ast.AssignFunc (v,[])) -> Some v
        |_ -> None
    )
    let (env:Map<Ast.Var, Core.Constr>) = constr |> List.mapi(fun i x -> x, Core.Constr i) |> Map.ofList
    let getConstr v =
        match env |> Map.tryFind v with
        |Some(c) -> c
        |None -> failwithf "Not a constr: %A" v
        

    let rec genExpr : Ast.Expr -> Core.Expr<Var> =
        function
        | Ast.Lit l -> genLit l
        | Ast.Var v -> Core.Var (Core.F (V v, 0))
        | Ast.Call(f, args) -> genCall ((Core.Var(Core.F (V f, 0)))) args
        | Ast.BinOp(x, f, y) -> genBinOp x y f
        | Ast.Match(e, cases) -> genMatch e cases
        | Ast.Block(block) -> genBlock None [] block
        | Ast.Prim (w, es) -> genPrim w es

    and genLit =
        function
        | Ast.Box(Ast.Integer i) -> Core.App(Core.Var(Core.F (IntConstr, 0)), [Core.Lit(Core.I32 i)])
        | Ast.Raw(l) -> Core.Lit(l)

    and genCall f =
        function
        | arg :: args -> genCall (Core.App(f, [genExpr arg])) args
        | [] -> f

    and genBinOp x y =
        function
        | Ast.Add -> genCall (Core.Var (Core.F (AddOp, 0))) [ x; y ]
        | Ast.Sub -> genCall (Core.Var (Core.F (SubOp, 0))) [ x; y ]

    and genMatchExpr = function
        |MatchExpr e -> genExpr e
        |MatchVar v -> Core.Var(Core.F v)

    and genMatch (e,t) cases =
        let manyCases = (cases |> List.map (fun (pat, expr) -> ([ pat ], expr)))
        genManyMatch
            ((Core.Unreachable)) [ MatchExpr e, t ] manyCases

    and genManyMatch def es (cases: (Ast.Pattern list * Ast.Expr) list) =
        match es with
        | (matchexpr, typ) :: es -> genCases (def, (matchexpr, typ), es, None) [] [] cases
        | [] ->
            match cases with
            | ([], e) :: _ -> genExpr e
            | [] -> def


    and genCases (def, (matchexpr, typ), es:(MatchExpr*Types.Typ) list, bind) subCases otherCases =
        function
        | (Ast.PatBind(bindV) :: xs, e) :: cases ->
            genCases (def, (matchexpr, typ), es, Some (V bindV, 0)) ((xs, e) :: subCases) otherCases cases
        | case :: cases -> genCases (def, (matchexpr, typ), es, bind) (subCases) (case :: otherCases) cases
        | [] ->
            let bindV = bind |> Option.defaultWith (fun () -> genVar Types.ValueT)
            let defVar = genVar Types.ValueT
            let defaultExpr = Core.closeE [bindV] (genManyMatch def es subCases)
            let d = Core.App(Core.Var (Core.F defVar), [Core.Var (Core.F bindV)])
            Core.letE
                (Core.Join, [defVar, Core.Lam([bindV], defaultExpr)],
                 genCasesWithDefault
                     (d, [], (matchexpr, typ), es, bindV)
                     otherCases)


    and genCasesWithDefault (def, alts, (matchexpr, typ), es, bind) cases =
        let state = (alts, (matchexpr, typ), es, def, bind)
        match cases with
        | ((Ast.PatLit(Ast.Raw l) :: _, _)) :: xs -> genPatLit state l [] [] cases
        | ((Ast.PatLit(Ast.Box(Ast.Integer _)) :: _, _)) :: xs ->
            genPatConstr state Core.IntDestr [ genVar Types.IntT, Types.IntT ] [] [] cases
        | ([ Ast.PatConstr(v, ps) ], e) :: xs ->
            let vs =
                ps
                |> List.map (function
                    | Ast.PatBind(v) -> V v, 0
                    | _ -> genVar Types.ValueT)
            genPatConstr state (getConstr v) (vs |> List.map(fun v -> v, Types.ValueT)) [] [] cases
        | (((Ast.PatBind(_) :: _), e)) :: xs -> genCasesWithDefault (def, alts, (matchexpr, typ), es, bind) xs
        | [] -> Core.caseE(genMatchExpr matchexpr, bind, ((Core.DefAlt, [], def)::alts))

    and genPatConstr state (v:Core.Constr) (vs: (Var*Types.Typ) list) (subCases: (Ast.Pattern list * Ast.Expr) list)
        otherCases =
        function
        | ((Ast.PatLit(Ast.Box(Ast.Integer i)) :: cases, e)) :: xs when v = Core.IntDestr ->
            let subCase = (Ast.PatLit(Ast.Raw(Core.I32 i))) :: cases, e
            genPatConstr state v vs (subCase :: subCases) otherCases xs
        | ((Ast.PatConstr(v1, vs1) :: cases, e)) :: xs when v = (getConstr v1) ->
            genPatConstr state v vs ((List.concat [ vs1; cases ], e) :: subCases) otherCases xs
        | x :: xs -> genPatConstr state v vs subCases (x :: otherCases) xs
        | [] ->
            let (alts, (matchexpr, typ), es, def, bind) = state
            let vsExprs = vs |> List.map (fun (v,t) -> (MatchVar (v), t))
            let subEs = List.concat [ vsExprs; es ]
            let e = genManyMatch def subEs subCases
            let alt = (Core.DataAlt (v), vs |> List.map fst, e)
            genCasesWithDefault (def, alt :: alts, (matchexpr, typ), es, bind) otherCases

    and genPatLit state (l: Core.Lit) (subCases: (Ast.Pattern list * Ast.Expr) list) otherCases =
        function
        | ((Ast.PatLit(Ast.Raw l1) :: cases, e)) :: xs when l = l1 ->
            genPatLit state l ((cases, e) :: subCases) otherCases xs
        | x :: xs -> genPatLit state l subCases (x :: otherCases) xs
        | [] ->
            let (alts, (matchexpr, typ), es, def, bind) = state
            let e = genManyMatch def es subCases
            let alt = (Core.LitAlt l, [], e)
            genCasesWithDefault (def, alt :: alts, (matchexpr, typ), es, bind) otherCases

    and genBlock returnValue lets =
        function
        | Ast.Assign(Ast.AssignVar(lhs), rhs) :: xs -> genBlock returnValue (((V lhs, 0), genRhsAssignVars (genExpr rhs) []) :: lets) xs
        | Ast.Assign(Ast.AssignFunc(lhs, args), rhs) :: xs -> genBlock returnValue (((V lhs, 0), genRhsAssignVars (genExpr rhs) args) :: lets) xs    
        | Ast.Return(e) :: xs ->
            match returnValue with
            | None -> genBlock (Some(genExpr e)) lets xs
            | Some _ -> failwith "Multiple return values"
        | [] ->
            match returnValue with
            | None -> failwith "No return value of block"
            | Some value -> Core.letE(Core.Rec, lets, value)


    and genRhsAssignVars rhs vars =
        genRhs rhs (vars |> List.map(function Ast.AssignVar v -> V v, 0))

    and genRhs (rhs: Core.Expr<Var>) =
        function
        | x :: xs -> Core.lamE([x], genRhs rhs xs)
        | [] -> rhs
        


    and genPrim w es : Core.Expr<Var> =
        Core.Prim(w, es |> List.map (fun v -> (V v, 0) |> Core.F |> Core.Var))

    let rec genExport call args rhs =
        match args with
        | x :: xs -> genExport (Core.App(call, [Core.App(Core.Var(Core.F (IntConstr, 0)), [Core.Var (Core.F x)])])) xs rhs
        | [] ->
            let resultVar = genVar Types.ValueT
            let returnVar = genVar Types.IntT
            Core.caseE
                (call, resultVar,
                     ([Core.DataAlt(Core.IntDestr), [ returnVar ], Core.Var(Core.F (returnVar)) ]))

    let rec genDeclarations topConstrs topExprs  =
        function
        | Ast.GlobalDecl(Ast.AssignVar(lhs), rhs)::xs -> 
            genDeclarations topConstrs (((V lhs, 0), (Core.NoExport, (genRhs (genExpr rhs) [])))::topExprs) xs
        | Ast.GlobalDecl(Ast.AssignFunc(lhs, args), rhs)::xs ->
            genDeclarations topConstrs (((V lhs, 0), (Core.NoExport, (genRhsAssignVars (genExpr rhs) args)))::topExprs) xs
        | Ast.ExportDecl((exportName, exportArgs), (lhs, args), rhs)::xs ->
            let exportTyp =
                Types.createFuncT Types.SatFunc (List.replicate (args |> List.length) Types.IntT) (Types.IntT)
            let exportVar = genVar exportTyp
            let exportArgs = exportArgs |> List.map (fun arg -> genVar Types.IntT) 
            genDeclarations topConstrs (
                ((V lhs, 0), (Core.NoExport, (genRhs (genExpr rhs) (args |> List.map (fun a -> V a, 0)))))::
                (exportVar, (Core.Export exportName, ((genRhs (genExport (Core.Var (Core.F (V lhs, 0))) exportArgs rhs) (exportArgs)))))::
                topExprs) xs   
        | Ast.TypeDecl(Ast.AssignFunc(v, vs))::xs -> 
            genDeclarations (((V v, 0), (getConstr v, vs |> List.map(function Ast.AssignVar v -> (V v, 0))))::topConstrs) topExprs xs
        |[] ->
            let constrs = List.append topConstrs BuiltIns.builtInConstrs
            let env = constrs |> List.mapi(fun i x -> x, i) |> Map.ofList
            let exprs =  List.append topExprs BuiltIns.builtInExprs
            let program = List.concat [
                constrs |> List.map (fun (b, (c, vs)) -> b, Core.TopConstr(c, vs))
                exprs |> List.map (fun (b, (export, e)) -> b, Core.TopExpr(export, e))
            ]
            Core.closeProgram program


    ast |> genDeclarations [] []