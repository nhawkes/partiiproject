module Core

type LocalVarId = int

type GlobalVarId = int


type DebugName = string


type InternalBinder<'b> = DebugName * 'b

type Lit = I32 of int32

type Constr =
    | IntConstr
    | BoolConstr
    | UserConstr of int

// A generalised de bruijn index
type Bound = int * int

// A name with an integer unique for that name within the expression
type UniqueName = string * int

type Var<'b> =
    // A bound variable
    | B of Bound
    // A free variable with data
    | F of UniqueName * 'b
    // A free variable without extra data
    | S of UniqueName

type Binder<'b> = UniqueName * 'b

let stringToName s = (s, 0)
let s2n s = stringToName s
let integer2Name i = ("gen", i)

type Let =
    // A variable binding
    | NonRec
    // A set of one or more variable binding that can depend on each other
    | Rec
    // A variable which is only tail called and does not escape meaning it does not allocate data on the heap.
    | Join

// Constants
type Const =
    // A data constructor
    | Constr of Constr
    // A literal
    | Lit of Lit
    // A primitive WebAssembly instruction
    | Prim of Wasm.Instr
    // The default case in a case expression
    | DefAlt

type Expr<'b when 'b: comparison> =
    // A loally nameless variable
    | Var of Var<'b>
    // A lambda function
    | Lam of InternalBinder<'b> * ClosedExpr<'b>
    // Bind a set of variables
    | Let of Let * Binds<'b> * ClosedExpr<'b>
    // A case expression
    | Case of Expr<'b> * InternalBinder<'b> * Alts<'b>
    // Call a lambda function
    | App of Expr<'b> * Expr<'b>
    // A constant
    | Const of Const
    // The unreachable WebAssembly instruction
    | Unreachable

// A list of bindings with variables and their expressions
and Binds<'b when 'b: comparison> = (InternalBinder<'b> * ClosedExpr<'b>) list

and ClosedExpr<'b when 'b: comparison> = Closed of Expr<'b>


and Alts<'b when 'b: comparison> = (Const * InternalBinder<'b> list * ClosedExpr<'b>) list

type Export =
    | Export of string
    | NoExport

type TopExpr<'b when 'b: comparison> = (UniqueName * 'b) * (Export * Expr<'b>)

type TopConstr<'b when 'b: comparison> = (UniqueName * 'b) * (Constr * (string * 'b) list)

type TopClosedExpr<'b when 'b: comparison> = (DebugName * 'b) * (Export * ClosedExpr<'b>)

type TopClosedConstr<'b when 'b: comparison> = (DebugName * 'b) * (Constr * (string * 'b) list)

type Program<'b when 'b: comparison> =
    { constrs: TopConstr<'b> list
      exprs: TopExpr<'b> list }

type ClosedProgram<'b when 'b: comparison> =
    { closedConstrs: TopClosedConstr<'b> list
      closedExprs: TopClosedExpr<'b> list }

let rec fvExpr =
    function
    | Var(F(v, _)) -> Set.singleton v
    | Var(S(v)) -> Set.singleton v
    | Var(_) -> Set.empty
    | Const(l) -> Set.empty
    | Lam(xs, e) -> fvClosed e
    | Let(l, bs, e) ->
        Set.union (fvClosed e)
            (bs
             |> List.map (snd >> fvClosed)
             |> Set.unionMany)
    | Case(e, b, alts) -> Set.union (fvExpr e) (fvClosedAlts alts)
    | App(a, b) -> Set.union (fvExpr a) (fvExpr b)
    | Unreachable -> Set.empty

and fvClosed (Closed e) = fvExpr e

and fvClosedAlts (alts) = fvAlts alts

and fvAlts alts =
    alts
    |> List.map (fun (_, _, e) -> fvClosed e)
    |> Set.unionMany

and fvProgram program = Set.unionMany (program.closedExprs |> List.map (fun (_, (_, closed)) -> fvClosed closed))


let rec bindClosed i bs (Closed(e)) = Closed(bindExpr (i + 1) bs e)

and bindExpr i bs =
    function
    | Var(B(i, j)) -> Var(B(i, j)): Expr<'a>
    | Var(F(v, _)) as v2 ->
        match bs |> List.tryFindIndex ((=) (v)) with
        | Some j -> Var(B(i, j))
        | _ -> v2
    | Var(S(v)) as v2 ->
        match bs |> List.tryFindIndex ((=) (v)) with
        | Some j -> Var(B(i, j))
        | _ -> v2
    | Const l -> Const l
    | Lam(xs, e) -> Lam(xs, bindClosed i bs e)
    | Let(l, binds, e) -> Let(l, binds |> List.map (fun (b, e) -> (b, bindClosed i bs e)), bindClosed i bs e)
    | Case(e, b, alts) -> Case(bindExpr i bs e, b, bindAlts i b bs alts)
    | App(a, b) -> App(bindExpr i bs a, bindExpr i bs b)
    | Unreachable -> Unreachable

and bindAlts i b bs alts = alts |> List.map (fun (alt, binds, e) -> (alt, binds, bindClosed i (bs) e))

let rec unbindClosed i bs (Closed(e)) = Closed(unbindExpr (i + 1) bs e)

and unbindExpr i bs =
    function
    | (Var(B(i2, j)): Expr<'a>) when i = i2 -> Var(F(bs |> List.item j))
    | Var(B(i2, j)) -> Var(B(i2, j))
    | Var(F(v, _) as f) ->
        if bs
           |> List.map fst
           |> List.contains v
        then failwith "Variable is not fresh"
        Var(f)
    | Var(S(v) as f) ->
        if bs
           |> List.map fst
           |> List.contains v
        then failwith "Variable is not fresh"
        Var(f)
    | Const l -> Const l
    | Lam(xs, e) -> Lam(xs, unbindClosed i bs e)
    | Let(l, binds, e) -> Let(l, binds |> List.map (fun (b, e) -> (b, unbindClosed i bs e)), unbindClosed i bs e)
    | Case(e, b, alts) -> Case(unbindExpr i bs e, b, unbindAlts i b bs alts)
    | App(a, b) -> App(unbindExpr i bs a, unbindExpr i bs b)
    | Unreachable -> Unreachable: Expr<'a>

and unbindAlts i b bs alts = alts |> List.map (fun (alt, binds, e) -> (alt, binds, unbindClosed i (bs) e))



let openE (bs) (Closed e) = unbindExpr 0 bs e

let closeE bs e =
    let closed = Closed(bindExpr 0 (bs) e)
    closed

let freshFromFrees frees debugName =
    let maxi =
        Seq.append [ -1 ]
            (frees
             |> Set.toSeq
             |> Seq.filter (fun (x, _) -> x = debugName)
             |> Seq.map snd)
        |> Seq.max
    (debugName, maxi + 1)

let freshen fv es (debugName: DebugName): UniqueName =
    let frees =
        es
        |> List.map fv
        |> Set.unionMany
    freshFromFrees frees debugName

type FreshSource =
    { next: DebugName -> UniqueName }

let freshSource fv es =
    let frees =
        ref
            (es
             |> List.map fv
             |> Set.unionMany)
    { next =
          (fun debugName ->
              let next = freshFromFrees !frees debugName
              frees := Set.add next !frees
              next) }



let freshenBinder fv es ((debugName, x): InternalBinder<_>) = freshen fv es debugName, x

let lamE (((s, i), b): Binder<_>, e) =
    let closed = closeE [ (s, i) ] e
    Lam((s, b), closed)

let caseE (e: Expr<_>, ((s, i), b): Binder<'a>, alts) =
    let closed =
        alts
        |> List.map (fun (alt, bs, e) ->
            (alt, bs |> List.map (fun ((s2, i2), b) -> (s2, b)), closeE ((s, i) :: (bs |> List.map fst)) e))
    Case(e, (s, b), closed)

let letE (l, binds: (Binder<_> * _) list, e) =
    let bs = binds |> List.map fst
    Let
        (l, binds |> List.map (fun (((s, i), b), e) -> ((s, b), closeE (bs |> List.map fst) e)),
         closeE (bs |> List.map fst) e)

let varE v = Var(F v)
let varS (v) = Var(S v)

let (|VarE|_|) =
    function
    | Var(F(name, b)) -> Some(name, b)
    | _ -> None

let (|VarS|_|) =
    function
    | Var(F(name, b)) -> Some(name)
    | Var(S(name)) -> Some(name)
    | _ -> None

let (|LamE|_|) =
    function
    | Lam(b, closed) ->
        let fb = freshenBinder fvClosed [ closed ] b
        let e = openE [ fb ] closed
        Some(fb, e)
    | _ -> None

let (|LetE|_|) =
    function
    | Let(l, xs: Binds<'a>, closed) ->
        let es = xs |> List.map snd
        let newBs = xs |> List.map (fun (x, e) -> (freshenBinder fvClosed (closed :: es) x, e))
        let e = openE (newBs |> List.map fst) closed
        let ys = newBs |> List.map (fun (x, e) -> x, openE (newBs |> List.map fst) e)
        Some(l, ys, e)
    | _ -> None

let (|CaseE|_|) =
    function
    | Case(e, b, alts) ->
        let bs = b :: (alts |> List.collect (fun (_, bs, _) -> bs))
        let es = alts |> List.map (fun (_, _, e) -> e)
        let newBs = bs |> List.map (fun x -> fst x, freshenBinder fvClosed (es) x)
        let freshMap = newBs |> Map.ofList

        let newAlts =
            alts
            |> List.map (fun (alt, bs, closed) ->
                let e = openE (newBs |> List.map (fun k -> Map.find (fst k) freshMap)) closed
                (alt, bs |> List.map (fun k -> Map.find (fst k) freshMap), e))
        Some(e, (freshMap |> Map.find (fst b)), newAlts)
    | _ -> None



let (|Program|) (program: ClosedProgram<'a>) =
    let constrs = program.closedConstrs |> List.map (fun (b, x) -> freshenBinder fvProgram [ program ] b, x)

    let exprs =
        program.closedExprs
        |> List.map (fun (b, x) ->
            let newB = freshenBinder fvProgram [ program ] b
            newB, x)

    let newBs =
        List.concat
            [ constrs |> List.map fst
              exprs |> List.map fst ]

    { constrs = constrs
      exprs = exprs |> List.map (fun (x, (export, e)) -> x, (export, openE (newBs) e)) }




let rec substClosed x1 e1 (Closed(e)) = Closed(substExpr x1 e1 e)

and substExpr x1 e1 =
    function
    | (Var(B(i, j)): Expr<'a>) -> Var(B(i, j))
    | Var(F(v, _) as f) when x1 = v -> e1
    | Var(S(v) as f) when x1 = v -> e1
    | Var(F(v, x) as f) -> Var f
    | Var(S(v) as f) -> Var f
    | Const l -> Const l
    | Lam(xs, e) -> Lam(xs, substClosed x1 e1 e)
    | Let(l, binds, e) -> Let(l, binds |> List.map (fun (b, e) -> (b, substClosed x1 e1 e)), substClosed x1 e1 e)
    | Case(e, b, alts) -> Case(substExpr x1 e1 e, b, substAlts x1 e1 alts)
    | App(a, b) -> App(substExpr x1 e1 a, substExpr x1 e1 b)
    | Unreachable -> Unreachable: Expr<'a>

and substAlts x1 e1 alts = alts |> List.map (fun (alt, binds, e) -> (alt, binds, substClosed x1 e1 e))

let rec substTopExprs x v =
    function
    | (b, (export, expr)) :: xs -> (b, (export, substExpr x v expr)) :: substTopExprs x v xs
    | [] -> []

let newline indent =
    "\n" + String.replicate (indent * 4) " "

let rec printExpr indent: Expr<'a> -> string =
    function
    | Var(v) -> printVar v
    | Const l -> printConst l
    | LamE(xs, e) -> sprintf "\<%s> -> %s" (xs |> printBinder) (printExpr indent e)
    | LetE(l, bs, e) ->
        sprintf "%s %s in %s%s" (printLet l) (printBinds (indent + 1) bs) (newline (indent + 1))
            (printExpr (indent + 1) e)
    | CaseE(e, b, alts) ->
        sprintf "case (%s) as %s of {%s%s}" (printExpr indent e) (printBinder b) (printAlts (indent + 1) alts)
            (newline indent)
    | App(a, b) -> sprintf "%s (%s)" (printExpr indent a) (printExpr indent b)
    | Unreachable -> "_|_"

and printConst =
    function
    | Constr c -> printConstr c
    | Lit l -> printLit l
    | Prim p -> sprintf "{%% %A %%}" p
    | DefAlt -> failwithf "Default can only appear in alts"

and printExprs indent exprs = String.concat ", " (exprs |> List.map (printExpr indent))

and printVar =
    function
    | B _ -> failwithf "Still bound"
    | F(s, x) -> printFrees [ s ]
    | S s -> printFrees [ s ]

and printVars vs = String.concat ", " (vs |> List.map printVar)

and printLet =
    function
    | NonRec -> "let"
    | Rec -> "let rec"
    | Join -> "let join"

and printBinds indent bs =
    (newline (indent)) + (bs
                          |> List.map (fun (b, e) -> printBind indent (b, e))
                          |> String.concat "and")

and printBind indent (b: UniqueName * 'a, e: Expr<_>) = sprintf "%s = %s" (b |> printBinder) (printExpr indent e)

and printAlts indent alts = (newline indent) + String.concat (newline indent) (alts |> List.map (printAlt indent))

and printAlt indent =
    function
    | Constr c, bs, e ->
        sprintf "%s(%s) -> %s" (printConstr c)
            (bs
             |> List.map printBinder
             |> String.concat ", ") (printExpr indent e)
    | Lit l, [], e -> sprintf "%s -> %s" (printLit l) (printExpr indent e)
    | DefAlt, [], e -> sprintf "_ -> %s" (printExpr indent e)

and printConstr =
    function
    | UserConstr i -> string (i)
    | IntConstr -> "Int"
    | BoolConstr -> "Bool"

and printLit = function
    | I32 i -> string (i)


and printFree ((s, i)) = sprintf "$%s.%i" s i

and printBinder ((x: UniqueName, _)) = printFree x

and printBinders bs =
    bs
    |> List.map fst
    |> List.map fst
    |> printFrees

and printFrees fs =
    fs
    |> List.map printFree
    |> String.concat ", "

and printProgram (Program program) =
    List.concat
        [ program.constrs
          |> List.map (fun (v, (c, vs)) ->
              sprintf "data %s = %s(%s)" (printBinder v) (printConstr c)
                  (vs
                   |> List.map fst
                   |> String.concat ", "))
          program.exprs
          |> List.map
              (fun (b, (export, e)) -> sprintf "%s = %s%s\n" (printBinder b) (newline 1) (printExpr 1 e)) ]
    |> String.concat "\n"

let rec mapExpr f: Expr<'a> -> Expr<'b> =
    function
    | Var(B(i, j)) -> Var(B(i, j))
    | Var(F(x, _)) -> Var(S(x))
    | Var(S x) -> Var(S x)
    | Const l -> Const l
    | LamE((x, b), e) -> lamE ((x, f b), mapExpr f e)
    | LetE(l, bs, e) -> letE (l, mapBinds f bs, mapExpr f e)
    | CaseE(e, (x, b), alts) -> caseE (mapExpr f e, (x, f b), mapAlts f alts)
    | App(a, b) -> App(mapExpr f a, mapExpr f b)
    | Unreachable -> Unreachable

and mapBinds f xs = xs |> List.map (fun ((x, b), e) -> ((x, f b), mapExpr f e))


and mapAlts f = function
    | (cases) -> cases |> List.map (mapAlt f)

and mapAlt f = function
    | ev, vs, e -> ev, vs |> List.map (fun (x, b) -> (x, f b)), mapExpr f e

let closeProgram program =
    let bs =
        List.concat
            [ program.constrs |> List.map fst
              program.exprs |> List.map fst ]
    let program =
        { closedConstrs = program.constrs |> List.map (fun (((s, i), b), (c, x)) -> (s, b), (c, x))
          closedExprs =
              program.exprs
              |> List.map (fun (((s, i), b), (export, e)) -> (s, b), (export, closeE (bs |> List.map fst) e)) }

    let frees = fvProgram program
    if frees |> Set.isEmpty
    then program
    else failwithf "Program has free variables: %A \nin \n%s" (frees) (printProgram program)
