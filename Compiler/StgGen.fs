module StgGen

let rec forcedCallArityWithKind k = function 
    |Types.FuncT(k2, a, b) when k = k2 ->
        1 + forcedCallArityWithKind k b    
    |_ -> 0

let forcedCallArity = function
    |Types.FuncT(k, a, b) ->
        match k with
        |Types.SatFunc -> forcedCallArityWithKind k b + 1 |> Some
        |Types.UnSatFunc -> None
    |_ -> 0 |> Some


let genProgram (core: Core.Program<Vars.Var, _>): Stg.Program<Vars.Var> =

    let globals = core |> List.map fst

    let addFree v frees =
        match globals |> List.contains v with
        | true -> frees
        | false -> v :: frees

    let genLit = function
        | Core.I32 i -> Wasm.I32Const i

    let rec genExpr e =
        let lf =
            match e with
            | Core.Var v -> genAppWithAtoms v []
            | Core.Lit lit -> Stg.lambdaForm (Stg.Prim [ Stg.ALit(genLit lit) ])
            | Core.Lam(v, e) -> genLam [ v ] e
            | Core.Let(bs, e) -> genLet e bs
            | Core.Case(e, v, alts) -> genCase e v alts
            | Core.App(a, b) -> genApp [ b ] a
            | Core.Prim (w, es) -> genPrim w es
            | Core.Unreachable -> genPrim Wasm.Unreachable [ Core.Lit(Core.I32 -1) ]
        Stg.normLf lf


    and genLam vs =
        function
        | Core.Lam(v, e) -> genLam (v :: vs) e
        | e ->
            let lf = genExpr e
            let frees = lf.frees |> List.filter (fun free -> not (vs |> List.contains free))
            { lf with
                  args = vs |> List.rev
                  frees = frees }

    and genLet e =
        function
        | Core.NonRec (v, e) -> genBindings genNonRec [ v, genExpr e ] (genExpr e)
        | Core.Join j -> genLetJoin j e
        | Core.Rec ls -> genBindings genRec (ls |> List.map(fun (v,e) -> v, genExpr e)) (genExpr e)
            
    and genNonRec (vs, expr) =
        match vs with
        |[] -> expr
        |v::vs -> Stg.Let(Stg.NonRec v, genNonRec (vs, expr))
     
    and genRec (vs, expr) = 
        match vs with
        |[] -> expr
        |vs -> Stg.Let(Stg.Rec vs, expr)

            
    and genBindings mapExpr lfEs lf =
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
                [ newLets
                  lf.lets ]

        let frees =
            lf.frees
            |> List.append innerFrees
            |> List.filter (fun v -> not(vs |> List.contains v))

        { lf with
              stdConstrs = stdConstrs
              frees = frees
              lets = lets
              expr = mapExpr(vs, lf.expr) }

    and genLetJoin (j, eJ) e =
        let lf = genExpr e
        let lfJ = genExpr eJ
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
            |> List.filter ((<>)j)

        let expr =
            Stg.Let(
                Stg.Join(j, args, lfJ.expr),
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

        





    and genCase e v alts =
        let lf = genExpr e
        let stgAlts, lfAlts = genAlts alts
        let lfCombined = Stg.combineLf lf lfAlts

        let expr = Stg.Case(lf.expr, v, stgAlts)
        let locals = v :: lfCombined.locals
        let frees = lfCombined.frees |> List.filter ((<>) v)
        { lfCombined with
              locals = locals
              frees = frees
              expr = expr }

    and genAlts =
        function
        | ((Core.DataAlt v, vs), e) :: xs, def -> 
            let lfDef = genExpr def
            genAAlt lfDef lfDef.expr [] v vs e xs
        | ((Core.LitAlt l, []), e) :: xs, def ->
            let lfDef = genExpr def
            genPAlt lfDef lfDef.expr [] l e xs
        | [], def ->
            let lfDef = genExpr def
            genPAlts lfDef lfDef.expr [] []

    and genAAlt lfAcc def aalts v vs e xs =
        let lfE = genExpr e
        let lfCombined = Stg.combineLf lfE lfAcc
        let frees = lfCombined.frees |> List.filter (fun free -> not (vs |> List.contains free))
        let locals = lfCombined.locals |> List.append vs

        let lf =
            { lfCombined with
                  frees = frees
                  locals = locals }
        genAAlts lf def
            (List.concat
                [ [ (v, vs), lf.expr ]
                  aalts ]) xs

    and genAAlts lfAcc def aalts =
        function
        | ((Core.DataAlt v, vs), e) :: xs -> genAAlt lfAcc def aalts v vs e xs
        | [] -> Stg.AAlts(aalts, def), lfAcc

    and genPAlt lfAcc def palts l e xs =
        let lfE = genExpr e
        let lfCombined = Stg.combineLf lfE lfAcc
        genPAlts lfCombined def
            (List.concat
                [ [ genLit l, lfCombined.expr ]
                  palts ]) xs

    and genPAlts lfAcc def (palts: Stg.PAlts<_>) =
        function
        | ((Core.LitAlt l, []), e) :: xs -> genPAlt lfAcc def palts l e xs
        | [] -> Stg.PAlts(palts, def), lfAcc

    and genApp args =
        function
        | Core.App(f, a) -> genApp (a :: args) f
        | Core.Var v -> genAppWithArgs v args

    and genAppWithArgs f args =
        let arity = forcedCallArity f.typ
        match arity with 
        |None -> 
            genAtoms (genAppWithAtoms f) [] args
        |Some i when i = (args |> List.length) -> 
            // Saturated
            genAtoms (fun xs -> Stg.lambdaForm (genCallWithAtoms f xs)) [] args
        |Some i when i < (args |> List.length) -> 
            // Oversaturated
            let firstArgs = args |> List.take i
            let secondArgs = args |> List.skip i
            let firstResult = genAppWithArgs f firstArgs
            genAtom (fun f -> genAppWithArgs f secondArgs) firstResult
        |Some i when i > (args |> List.length) -> 
            // Undersaturated
            let extraArgs = [(args |> List.length)..i] |> List.map (fun _ -> Vars.generateVar Types.ValueT)
            let lf = genAppWithArgs f (List.concat [args; extraArgs |> List.map Core.Var])
            let frees = lf.frees |> List.filter(fun v -> not(extraArgs |> List.contains(v)))
            {lf with args=List.concat[extraArgs; lf.args]; frees=frees}
        
    and genAppWithAtoms (f:Vars.Var) atoms =
        let e = genCallWithAtoms f atoms
        let lf = Stg.lambdaForm e
        { lf with frees = lf.frees |> addFree f }

    and genCallWithAtoms (f:Vars.Var) atoms =   
        match f.callType with
        |Some Vars.JoinCall -> Stg.Jump(f, atoms)
        |Some Vars.DirectCall -> Stg.Call(f, atoms)
        |Some Vars.ConstrCall -> Stg.Constr(f, atoms)
        |_ ->
        match f.typ with
        | Types.ValueT -> Stg.App(f, atoms)
        | Types.FuncT _ -> Stg.App(f, atoms)
        | Types.IntT -> 
            if atoms |> List.isEmpty then
                Stg.Prim [ Stg.AVar f ]
            else failwith "Literal cannot take arguments"


    and genAtoms mapAtoms xs =
        function
        | (Core.Var arg) :: args ->
            let (lf:Stg.LambdaForm<_>) = genAtoms mapAtoms (Stg.AVar arg :: xs) args
            let frees = lf.frees |> addFree arg
            { lf with frees = frees }
        | (Core.Lit arg) :: args -> genAtoms mapAtoms (Stg.ALit(genLit arg) :: xs) args
        | arg :: args ->
            genAtom (fun var -> genAtoms mapAtoms (Stg.AVar var :: xs) args) (genExpr arg)
        | [] -> mapAtoms (xs |> List.rev)

    and genAtom f (arg) =    
        let var = Vars.generateVar Types.ValueT
        let lfInner = f var
        let lfE = arg
        genBindings genNonRec [var, lfE] lfInner

    and genPrim w ps =
        genAtoms (fun atoms -> Stg.lambdaForm (Stg.Prim(Stg.ALit w::atoms))) [] ps


    let genTopLevel =
        function
        | b, Core.TopExpr e when (b:Vars.Var).typ = Types.ValueT -> b, Stg.TopCaf(genExpr e)
        | b, Core.TopExpr e -> b, Stg.TopLam(genExpr e)
        | b, Core.TopConstr vs -> b, Stg.TopConstr vs

    let program = core |> List.map genTopLevel
    program
