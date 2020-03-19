module CoreGen

open Vars

type MatchExpr =
    | MatchExpr of Ast.Expr
    | MatchVar of Core.Binder<Var>


let var: Ast.Var -> Core.Var<_> =
    fun v ->
        Core.F
            (Core.s2n v.text,
             { v = Some v
               typ = Types.ValueT
               hintInline = false })

let b: Ast.Var -> Core.Binder<_> =
    fun v ->
        (Core.s2n v.text,
         { v = Some v
           typ = Types.ValueT
           hintInline = false })

let genName =
    let i = ref (1)
    fun () ->
        let next = !i
        i := !i + 1
        Core.integer2Name next


let genProgram (ast: Ast.Program): Core.ClosedProgram<_> =
    let constr =
        ast
        |> List.choose (function
            | Ast.TypeDecl(Ast.AssignVar v) -> Some v.text
            | Ast.TypeDecl(Ast.AssignFunc(v, _)) -> Some v.text
            | _ -> None)

    let (env: Map<string, Core.Constr>) =
        constr
        |> List.mapi (fun i x -> x, Core.UserConstr i)
        |> Map.ofList

    let getConstr (v: Ast.Var) =
        match env |> Map.tryFind v.text with
        | Some(c) -> c
        | None -> failwithf "Not a constr: %A" v.text


    let rec genExpr: Ast.Expr -> Core.Expr<Var> =
        function
        | Ast.Lit l -> genLit l
        | Ast.Var v -> Core.Var(var v)
        | Ast.Call(f, args) -> genCall ((Core.Var(var f))) args
        | Ast.BinOp(x, f, y) -> genBinOp x y f
        | Ast.Match(e, cases) -> genMatch e cases
        | Ast.Block(block) -> genBlock None [] block
        | Ast.If(cond, (ifTrue, ifFalse)) -> genIf cond ifTrue ifFalse

    and genLit =
        function
        | Ast.Box(Ast.Integer i) -> Core.App(Core.Var(Core.F(intConstr)), Core.Const <| Core.Lit(Core.I32 i))
        | Ast.Raw(l) -> Core.Const <| Core.Lit(l)

    and genCall f =
        function
        | arg :: args -> genCall (Core.App(f, genExpr arg)) args
        | [] -> f

    and genBinOp x y =
        function
        | Ast.Add -> genCall (Core.Var(Core.F(addOp))) [ x; y ]
        | Ast.Sub -> genCall (Core.Var(Core.F(subOp))) [ x; y ]
        | Ast.Mul -> genCall (Core.Var(Core.F(mulOp))) [ x; y ]
        | Ast.Div -> genCall (Core.Var(Core.F(divOp))) [ x; y ]
        | Ast.Equals -> genCall (Core.Var(Core.F(equalsOp))) [ x; y ]
        | Ast.LessThan -> genCall (Core.Var(Core.F(lessThanOp))) [ x; y ]

    and genMatchExpr =
        function
        | MatchExpr e -> genExpr e
        | MatchVar v -> Core.varE (v)

    and genMatch (e, t) cases =
        let manyCases = (cases |> List.map (fun (pat, expr) -> ([ pat ], expr)))
        genManyMatch ((Core.Unreachable)) [ MatchExpr e, t ] manyCases

    and genManyMatch def es (cases: (Ast.Pattern list * Ast.Expr) list) =
        match es with
        | (matchexpr, typ) :: es -> genCases (def, (matchexpr, typ), es, None) [] [] cases
        | [] ->
            match cases with
            | ([], e) :: _ -> genExpr e
            | [] -> def


    and genCases (def, (matchexpr, typ), es: (MatchExpr * Types.Typ) list, bind) subCases otherCases =
        function
        | (Ast.PatBind(bindV) :: xs, e) :: cases ->
            genCases
                (def, (matchexpr, typ), es,
                 Some
                     (Core.s2n bindV.text,
                      { v = Some bindV
                        typ = Types.ValueT
                        hintInline = false })) ((xs, e) :: subCases) otherCases cases
        | case :: cases -> genCases (def, (matchexpr, typ), es, bind) (subCases) (case :: otherCases) cases
        | [] ->
            let bindV =
                bind
                |> Option.defaultWith (fun () ->
                    genName(),
                    { v = None
                      typ = Types.ValueT
                      hintInline = false })
            let defVar =
                genName(),
                { v = None
                  typ = Types.ValueT
                  hintInline = false }

            let defaultExpr = Core.closeE [ bindV |> fst ] (genManyMatch def es subCases)
            let d = Core.App(Core.varE (defVar), Core.varE bindV)
            Core.letE
                (Core.Join, [ (defVar), Core.Lam(("", snd bindV), defaultExpr) ],
                 genCasesWithDefault (d, [], (matchexpr, typ), es, bindV) otherCases)


    and genCasesWithDefault (def, alts, (matchexpr, typ), es, bind) cases =
        let state = (alts, (matchexpr, typ), es, def, bind)
        match cases with
        | ((Ast.PatLit(Ast.Raw l) :: _, _)) :: xs -> genPatLit state l [] [] cases
        | ((Ast.PatLit(Ast.Box(Ast.Integer _)) :: _, _)) :: xs ->
            genPatConstr state Core.IntConstr
                [ (genName(),
                   { v = None
                     typ = Types.IntT
                     hintInline = false }), Types.IntT ] [] [] cases
        | ([ Ast.PatConstr(v, ps) ], e) :: xs ->
            let vs =
                ps
                |> List.map (function
                    | Ast.PatBind(v) -> b v
                    | _ ->
                        genName(),
                        { v = None
                          typ = Types.ValueT
                          hintInline = false })
            genPatConstr state (getConstr v) (vs |> List.map (fun v -> v, Types.ValueT)) [] [] cases
        | (((Ast.PatBind(_) :: _), e)) :: xs -> genCasesWithDefault (def, alts, (matchexpr, typ), es, bind) xs
        | [] -> Core.caseE (genMatchExpr matchexpr, bind, ((Core.DefAlt, [], def) :: alts))

    and genPatConstr
        state
        (v: Core.Constr)
        (vs: (Core.Binder<_> * Types.Typ) list)
        (subCases: (Ast.Pattern list * Ast.Expr) list)
        otherCases
        =
        function
        | ((Ast.PatLit(Ast.Box(Ast.Integer i)) :: cases, e)) :: xs when v = Core.IntConstr ->
            let subCase = (Ast.PatLit(Ast.Raw(Core.I32 i))) :: cases, e
            genPatConstr state v vs (subCase :: subCases) otherCases xs
        | ((Ast.PatConstr(v1, vs1) :: cases, e)) :: xs when v = (getConstr v1) ->
            genPatConstr state v vs ((List.concat [ vs1; cases ], e) :: subCases) otherCases xs
        | x :: xs -> genPatConstr state v vs subCases (x :: otherCases) xs
        | [] ->
            let (alts, (matchexpr, typ), es, def, bind) = state
            let vsExprs = vs |> List.map (fun (v, t) -> (MatchVar(v), t))
            let subEs = List.concat [ vsExprs; es ]
            let e = genManyMatch def subEs subCases
            let alt = (Core.Constr(v), vs |> List.map fst, e)
            genCasesWithDefault (def, alt :: alts, (matchexpr, typ), es, bind) otherCases

    and genPatLit state (l: Core.Lit) (subCases: (Ast.Pattern list * Ast.Expr) list) otherCases =
        function
        | ((Ast.PatLit(Ast.Raw l1) :: cases, e)) :: xs when l = l1 ->
            genPatLit state l ((cases, e) :: subCases) otherCases xs
        | x :: xs -> genPatLit state l subCases (x :: otherCases) xs
        | [] ->
            let (alts, (matchexpr, typ), es, def, bind) = state
            let e = genManyMatch def es subCases
            let alt = (Core.Lit l, [], e)
            genCasesWithDefault (def, alt :: alts, (matchexpr, typ), es, bind) otherCases

    and genBlock returnValue lets =
        function
        | Ast.Assign(Ast.AssignVar(lhs), rhs) :: xs ->
            genBlock returnValue (((b <| lhs), genRhsAssignVars (genExpr rhs) []) :: lets) xs
        | Ast.Assign(Ast.AssignFunc(lhs, args), rhs) :: xs ->
            genBlock returnValue (((b <| lhs), genRhsAssignVars (genExpr rhs) args) :: lets) xs
        | Ast.Return(e) :: xs ->
            match returnValue with
            | None -> genBlock (Some(genExpr e)) lets xs
            | Some _ -> failwith "Multiple return values"
        | [] ->
            match returnValue with
            | None -> failwith "No return value of block"
            | Some value -> Core.letE (Core.Rec, lets, value)


    and genRhsAssignVars rhs vars =
        genRhs rhs
            (vars
             |> List.map (function
                 | Ast.AssignFunc(f, xs) ->
                     let name, x = b f
                     name, { x with typ = Types.createFuncT (xs |> List.map (fun _ -> Types.ValueT)) Types.ValueT }
                 | Ast.AssignVar v -> b v))

    and genRhs (rhs: Core.Expr<_>) =
        function
        | x :: xs -> Core.lamE ((x), genRhs rhs xs)
        | [] -> rhs




    and genIf cond ifTrue ifFalse =
        let caseVar1 =
            genName(),
            { v = None
              typ = Types.ValueT
              hintInline = false }

        let caseVar2 =
            genName(),
            { v = None
              typ = Types.ValueT
              hintInline = false }

        let (boolName, boolVar) =
            genName(),
            { v = None
              typ = Types.IntT
              hintInline = false }

        Core.caseE
            (genExpr cond, caseVar1,
             [ Core.Constr Core.BoolConstr, [ (boolName, boolVar) ],
               Core.caseE
                   (Core.varS boolName, caseVar2,
                    [ Core.DefAlt, [], genExpr ifTrue
                      Core.Lit(Core.I32 0), [], genExpr ifFalse ]) ])

    let rec genExport call args rhs =
        match args with
        | x :: xs -> genExport (Core.App(call, Core.App(Core.Var(Core.F intConstr), Core.Var(Core.F x)))) xs rhs
        | [] ->
            let resultVar =
                genName(),
                { v = None
                  typ = Types.ValueT
                  hintInline = false }

            let returnVar =
                genName(),
                { v = None
                  typ = Types.IntT
                  hintInline = false }

            Core.caseE (call, resultVar, ([ Core.Constr(Core.IntConstr), [ returnVar ], Core.Var(Core.F(returnVar)) ]))

    let rec genDeclarations fresh topConstrs topExprs =
        function
        | Ast.GlobalDecl(Ast.AssignVar(lhs), rhs) :: xs ->
            genDeclarations fresh topConstrs (((b lhs), (Core.NoExport, (genRhs (genExpr rhs) []))) :: topExprs) xs
        | Ast.GlobalDecl(Ast.AssignFunc(lhs, args), rhs) :: xs ->
            genDeclarations fresh topConstrs
                (((b lhs), (Core.NoExport, (genRhsAssignVars (genExpr rhs) args))) :: topExprs) xs
        | Ast.ExportDecl((exportName, exportArgs), (lhs, args), rhs) :: xs ->
            let exportTyp = Types.createFuncT (List.replicate (args |> List.length) Types.IntT) (Types.IntT)

            let exportVar =
                genName(),
                { v = None
                  typ = exportTyp
                  hintInline = false }

            let exportArgs =
                exportArgs
                |> List.mapi (fun i arg ->
                    Core.integer2Name (i + fresh),
                    { v = None
                      typ = Types.IntT
                      hintInline = false })

            genDeclarations (fresh + 1 + exportArgs.Length) topConstrs
                ((b lhs, (Core.NoExport, (genRhs (genExpr rhs) (args |> List.map (b)))))
                 :: (exportVar,
                     (Core.Export exportName,
                      ((genRhs (genExport (Core.Var(var lhs)) (exportArgs) rhs) (exportArgs))))) :: topExprs) xs
        | Ast.TypeDecl(Ast.AssignFunc(v, vs)) :: xs ->
            genDeclarations fresh
                ((b v,
                  (getConstr v,
                   vs |> List.map (function
                             | Ast.AssignVar v ->
                                 v.text,
                                 { v = Some v
                                   typ = Types.ValueT
                                   hintInline = false }))) :: topConstrs) topExprs xs
        | [] ->
            let constrs = List.append topConstrs BuiltIns.builtInConstrs

            let env =
                constrs
                |> List.mapi (fun i x -> x, i)
                |> Map.ofList

            let exprs = List.append topExprs BuiltIns.builtInExprs
            Core.closeProgram
                { constrs = constrs
                  exprs = exprs }


    ast |> genDeclarations 0 [] []
