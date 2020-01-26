module Core

type Lit = I32 of int32

type Constr =
    | FreeConstr of string
    | IntDestr
    | Constr of int

type Var<'b> =
    | B of int * int
    | F of 'b

type Let =
    | Rec
    | NonRec
    | Join

type Expr<'b when 'b: comparison> =
    | Var of Var<'b>
    | Lit of Lit
    | Lam of 'b list * Closed<'b>
    | Let of Let * Binds<'b> * Closed<'b>
    | Case of Expr<'b> * 'b * Alts<'b>
    | App of Expr<'b> * Expr<'b> list
    | Prim of Wasm.Instr * Expr<'b> list
    | Unreachable

and Closed<'b when 'b: comparison> = Closed of Expr<'b>

and Binds<'b when 'b: comparison> = ('b * Closed<'b>) list

and Pat =
    | DataAlt of Constr
    | LitAlt of Lit
    | DefAlt


and Alts<'b when 'b: comparison> = ClosedAlts of (Pat * 'b list * Closed<'b>) list

type Export =
    |Export
    |NoExport

type TopLevel<'b when 'b: comparison> =
    | TopExpr of Export * Expr<'b>
    | TopConstr of Constr * 'b list

type TopLevelClosed<'b when 'b: comparison> =
    | ClosedTopExpr of Export * Closed<'b>
    | ClosedTopConstr of Constr * 'b list

type TopBind<'b when 'b: comparison> = 'b * TopLevel<'b>
type TopBindClosed<'b when 'b: comparison> = 'b * TopLevelClosed<'b>

type Program<'b when 'b: comparison> = TopBind<'b> list
type ClosedProgram<'b when 'b: comparison> = TopBindClosed<'b> list

type Name<'a> = 'a * int
let name<'a> (s:'a, i:int) = sprintf "%A" s, i

let rec fvExpr = function
    | Var(F v) -> Set.singleton v
    | Var(_) -> Set.empty
    | Lit(l) -> Set.empty
    | Lam(xs, e) -> fvClosed e
    | Let(l, bs, e) -> Set.union (fvClosed e) (bs |> List.map (snd >> fvClosed) |> Set.unionMany)
    | Case(e, b, alts) -> Set.union (fvExpr e) (fvClosedAlts alts)
    | App(f, xs) -> Set.union (fvExpr f) (xs |> List.map (fvExpr) |> Set.unionMany)
    | Prim(p, es) -> es |> List.map (fvExpr) |> Set.unionMany
    | Unreachable -> Set.empty

and fvClosed (Closed e) = fvExpr e
and fvClosedAlts (ClosedAlts alts) =
    fvAlts alts
and fvAlts alts =
    alts |> List.map(fun (_,_,e)->fvClosed e) |> Set.unionMany

and fvTopLevel = function
    |b, ClosedTopExpr(export, closed) -> fvClosed closed
    |b, ClosedTopConstr(c, bs) -> Set.empty
    
and fvProgram program =
    Set.unionMany (program |> List.map fvTopLevel)


let rec bindClosed i bs (Closed(e)) =
    Closed(bindExpr (i+1) bs e)

and bindExpr i bs =
    function
    | Var (B (i,j)) -> Var(B (i,j))
    | Var(F v) ->
        match bs |> List.tryFindIndex ((=)(name v)) with
        | Some j -> Var(B(i, j))
        | _ -> Var(F v)
    | Lit l -> Lit l
    | Lam(xs, e) -> Lam(xs, bindClosed i bs e)
    | Let(l, binds, e) -> Let(l, binds |> List.map(fun (b,e) -> (b, bindClosed i bs e)), bindClosed i bs e)
    | Case(e, b, alts) -> Case(bindExpr i bs e, b, bindClosedAlts i bs alts)
    | App(f, xs) -> App(bindExpr i bs f, xs |> List.map (bindExpr i bs))
    | Prim(p, es) -> Prim(p, es |> List.map (bindExpr i bs))
    | Unreachable -> Unreachable

and bindAlts i bs alts =
    alts |> List.map(fun (alt, binds, e) -> (alt, binds, bindClosed i bs e))

and bindClosedAlts i bs (ClosedAlts alts) =    
    ClosedAlts(bindAlts (i+1) bs alts)

let rec unbindClosed i bs (Closed(e)) =
    Closed(unbindExpr (i+1) bs e)
    
and unbindExpr i bs =
    function
    | Var (B (i2,j)) when i = i2 ->
        Var (F (bs |> List.item j))  
    | Var (B (i2,j)) -> Var (B (i2,j))       
    | Var(F v) ->
        if bs |> List.contains v then failwith "Variable is not fresh"
        Var(F v)
    | Lit l -> Lit l
    | Lam(xs, e) -> Lam(xs, unbindClosed i bs e)
    | Let(l, binds, e) -> Let(l, binds |> List.map(fun (b,e) -> (b, unbindClosed i bs e)), unbindClosed i bs e)
    | Case(e, b, alts) -> Case(unbindExpr i bs e, b, unbindClosedAlts i bs alts)
    | App(f, xs) -> App(unbindExpr i bs f, xs |> List.map (unbindExpr i bs))
    | Prim(p, es) -> Prim(p, es |> List.map (unbindExpr i bs))
    | Unreachable -> Unreachable

and unbindClosedAlts i bs (ClosedAlts alts) =
    ClosedAlts(unbindAlts (i+1) bs alts)

and unbindAlts i bs alts =
    alts |> List.map(fun (alt, binds, e) -> (alt, binds, unbindClosed i bs e))


let openE (bs) (Closed e) =
    let fs = bs |> List.map fst
    let frees = fvExpr e
    let maxi = Seq.append [0] (frees |> Set.toSeq |> Seq.filter(fun (x,_) -> fs |> List.contains x) |> Seq.map snd) |> Seq.max
    let bsFresh = bs |> List.map(fun (x, _) -> x, maxi+1)
    bsFresh, unbindExpr 0 bsFresh e

let openAlts bs (ClosedAlts alts) =
    let fs = bs |> List.map fst
    let frees = fvAlts alts
    let maxi = Seq.append [0] (frees |> Set.toSeq |> Seq.filter(fun (x,_) -> fs |> List.contains x) |> Seq.map snd) |> Seq.max
    let bsFresh = bs |> List.map(fun (x, _) -> x, maxi+1)
    bsFresh, unbindAlts 0 bsFresh alts

let closeE bs e =
    let closed = Closed(bindExpr 0 (bs |> List.map name) e)
    closed

let closeAlts bs alts =
    let closedAlts = ClosedAlts(bindAlts 0 (bs |> List.map name) alts)
    closedAlts


    
let lamE (xs, e) =
    let closed = closeE xs e
    Lam(xs, closed) 

let caseE (e, b, alts) =
    let closed = alts |> List.map (fun (alt, bs, e) -> (alt, bs, closeE (bs) e))   
    Case(e, b, closeAlts [b] closed)
    
let letE (l, binds, e) =
    let bs = binds |> List.map fst
    Let(l, binds |> List.map(fun (b,e) -> (b, closeE bs e)), closeE bs e)

let (|LamE|_|) = function
    |Lam(bs, closed) -> 
        let newBs, e = openE bs closed
        Some(newBs, e)
    |_ -> None
let (|LetE|_|) = function
    |Let(l, bs, closed) -> 
        let newBs, e = openE (bs|>List.map fst) closed
        Some(l, newBs, e)
    |_ -> None
let (|CaseE|_|) = function
    |Case(e, b, alts) -> 
        let [newB], closedAlts = openAlts [b] alts
        let newAlts = closedAlts |> List.map(fun (alt, bs, closed) -> 
            let newBs, e = openE bs closed
            (alt, newBs, e)        
        )
        Some(e, newB, newAlts)
    |_ -> None
let closeProgram program = 
    let bs = program |> List.map fst
    program |> List.map (function
        |b, TopExpr(export, e) -> b, ClosedTopExpr(export, closeE bs e)
        |b, TopConstr(c, bs) -> b, ClosedTopConstr(c, bs)
    ) 

let (|Program|) program =
    let bs = program |> List.map fst
    program |> List.map (function
        |b, ClosedTopExpr(export, e) -> b, TopExpr(export, openE bs e)
        |b, ClosedTopConstr(c, bs) -> b, TopConstr(c, bs)
    )    

    




let newline indent = "\n" + String.replicate (indent*4) " "

let rec printExpr indent = function
    | Var(v) -> printVar v
    | Lit(I32 i) -> sprintf "%i" i
    | LamE(xs, e) -> sprintf "<%s> -> %s" (xs |> List.map printBinder |> String.concat ", ") (printExpr indent e)
    | LetE(l, bs, e) -> sprintf "%s %s%s in %s%s" (printLet l) (newline indent)  (printBinds indent bs) (newline indent)  (printExpr indent e)
    | CaseE(e, b, alts) -> sprintf "case %s as %s of {%s%s}" (printExpr indent e) (printBinder b) (printAlts (indent+1) alts) (newline indent)
    | App(f, xs) -> sprintf "%s (%s)" (printExpr indent f) (printExprs indent xs)
    | Prim(p, es) -> sprintf "{%% %A %s %%}" p (printExprs indent es)
    | Unreachable -> "_|_"

and printExprs indent exprs =
    String.concat ", " (exprs |> List.map (printExpr indent))

and printVar = function
    |B _ -> failwithf "Still bound"
    |F s -> printBinder s        

and printVars vs = 
    String.concat ", " (vs |> List.map printVar) 

and printLet = function
    |NonRec -> "let"
    |Rec -> "let rec"
    |Join -> "let join"

and printBinds indent bs =
    bs |> 
        List.map(fun (b, e) -> printBind indent (b, e)) |>
        String.concat "and"

and printBind indent (b, e:Expr<_>) =
    sprintf "%s = %s" (b |> printBinder) (printExpr indent e)

and printAlts indent alts =
    (newline indent) + String.concat (newline indent) (alts |> List.map (printAlt indent))

and printAlt indent = function
    |DataAlt c, bs, e -> 
        sprintf "%s(%s) -> %s" (printConstr c) (bs |> List.map printBinder |> String.concat ", ") (printExpr indent e)
    |LitAlt l, [], e -> 
        sprintf "%s -> %s" (printLit l) (printExpr indent e)
    |DefAlt, [], e -> 
        sprintf "_ -> %s" (printExpr indent e)

and printConstr = function
    |FreeConstr s -> s
    |Constr i -> string(i)
    |IntDestr -> "Int"

and printLit = function
    |I32 i -> string(i)    


and printBinder (s, i) = sprintf "%A_%i" s i 
and printBinders bs = bs |> List.map printBinder |> String.concat ", " 

and printProgram program = 
    let bs = program |> List.map fst
    program |> List.map (printTopLevel bs) |> String.concat "\n" 

and printTopLevel bs = function
    |b, TopExpr(export, closed) -> sprintf "%s = %s%s\n" (printBinder b) (newline 1) (printClosed 1 bs closed)
    |b, TopConstr(c, bs) -> sprintf "Data %s = %s(%s)" (printBinder b) (printConstr c) (printBinders bs)


(*
let rec mapExpr f: Expr<'a> -> Expr<'b> =
    function
    | Var v -> Var v
    | Lit l -> Lit l
    | Lam(b, e) -> Lam(f b, mapExpr f e)
    | Let(bs, e) -> Let(mapBinds f bs, mapExpr f e)
    | Case(e, b, alts) -> Case(mapExpr f e, f b, mapAlts f alts)
    | App(a, b) -> App(mapExpr f a, mapExpr f b)
    | Prim(w, es) -> Prim(w, es |> List.map (mapExpr f))
    | Unreachable -> Unreachable

and mapBinds f =
    function
    | Rec bs -> Rec(bs |> List.map (fun (b, e) -> (f b, mapExpr f e)))
    | NonRec(b, e) -> NonRec(f b, mapExpr f e)
    | Join(b, e) -> Join(f b, mapExpr f e)


and mapAlts f = function
    | (cases, def) -> cases |> List.map (mapAlt f), mapExpr f def

and mapAlt f = function
    | (ev, vs), e -> (mapPat f ev, vs |> List.map f), mapExpr f e

and mapPat f =
    function
    | DataAlt v -> DataAlt v
    | LitAlt l -> LitAlt l

let mapTopLevel f =
    function
    | TopExpr e -> TopExpr(mapExpr f e)
    | TopConstr vs -> TopConstr(vs |> List.map f)

let mapTopBind f (b, e) = (f b, mapTopLevel f e)

let mapProgram f p = p |> List.map (mapTopBind f)


let rec mapVarsExpr f: Expr<'u, 'b> -> Expr<'b> =
    function
    | Var v -> Var(f v)
    | Lit l -> Lit l
    | Lam(b, e) -> Lam(b, mapVarsExpr f e)
    | Let(bs, e) -> Let(mapVarsBinds f bs, mapVarsExpr f e)
    | Case(e, b, alts) -> Case(mapVarsExpr f e, b, mapVarsAlts f alts)
    | App(a, b) -> App(mapVarsExpr f a, mapVarsExpr f b)
    | Prim(w, es) -> Prim(w, es |> List.map (mapVarsExpr f))
    | Unreachable -> Unreachable

and mapVarsBinds f =
    function
    | Rec bs -> Rec(bs |> List.map (fun (b, e) -> (b, mapVarsExpr f e)))
    | NonRec(b, e) -> NonRec(b, mapVarsExpr f e)
    | Join(b, e) -> Join(b, mapVarsExpr f e)


and mapVarsAlts f = function
    | (cases, def) -> cases |> List.map (mapVarsAlt f), mapVarsExpr f def

and mapVarsAlt f = function
    | (ev, vs), e -> (mapVarsPat f ev, vs), mapVarsExpr f e

and mapVarsPat f =
    function
    | DataAlt v -> DataAlt(f v)
    | LitAlt l -> LitAlt l

and mapVarsPrim f =
    function
    | AVar v -> AVar(f v)
    | ALit l -> ALit l
    | AWasm w -> AWasm w

let mapVarsTopLevel f =
    function
    | TopExpr e -> TopExpr(mapVarsExpr f e)
    | TopConstr vs -> TopConstr(vs |> List.map f)

let mapVarsTopBind f (b, e) = (f b, mapVarsTopLevel f e)

let mapVarsProgram f p = p |> List.map (mapVarsTopBind f)


*)

