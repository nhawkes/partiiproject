module Core

type Lit = I32 of int32

type Prim<'v> =
    |AWasm of Wasm.Instr
    |ALit of Lit
    |AVar of 'v

type Expr<'v, 'b> =
    | Var of 'v
    | Lit of Lit
    | Lam of 'b * Expr<'v, 'b>
    | Let of Binds<'v, 'b> * Expr<'v, 'b>
    | Case of Expr<'v, 'b> * 'b * Alts<'v, 'b>
    | App of Expr<'v, 'b> * Expr<'v, 'b>
    | Prim of Prim<'v> list
    | Unreachable

and Binds<'v, 'b> =
    | Rec of Bind<'v, 'b> list
    | NonRec of Bind<'v, 'b>
    | Join of Bind<'v, 'b>
 
and Pat<'v> =
    |DataAlt of 'v
    |LitAlt of Lit

and Bind<'v, 'b> = 'b * Expr<'v, 'b>

and Default<'v, 'b> = Expr<'v, 'b>


and Alts<'v, 'b> = ((Pat<'v> * 'b list) * Expr<'v, 'b>) list * Default<'v, 'b>

type TopLevel<'v, 'b> =
    | TopExpr of Expr<'v, 'b>
    | TopConstr of 'b list

type TopBind<'v, 'b> = 'b * TopLevel<'v, 'b>

type Program<'v, 'b> = TopBind<'v, 'b> list



let rec mapExpr f : Expr<'v, 'a> -> Expr<'v, 'b> =
    function
    | Var v -> Var v
    | Lit l -> Lit l
    | Lam(b, e) -> Lam(f b, mapExpr f e)
    | Let(bs, e) -> Let(mapBinds f bs, mapExpr f e)
    | Case(e, b, alts) -> Case(mapExpr f e, f b, mapAlts f alts)
    | App(a, b) -> App(mapExpr f a, mapExpr f b)
    | Prim ps -> Prim(ps |> List.map (mapPrim f))
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

and mapPat f = function
    |DataAlt v -> DataAlt v
    |LitAlt l -> LitAlt l
    
and mapPrim f =
    function
    | AVar v -> AVar v
    | ALit l -> ALit l
    | AWasm w -> AWasm w

let mapTopLevel f =
    function
    | TopExpr e -> TopExpr(mapExpr f e)
    | TopConstr vs -> TopConstr(vs |> List.map f)

let mapTopBind f (b, e) = (f b, mapTopLevel f e)

let mapProgram f p = p |> List.map (mapTopBind f)


let rec mapVarsExpr f : Expr<'u, 'b> -> Expr<'v, 'b> =
    function
    | Var v -> Var (f v)
    | Lit l -> Lit l
    | Lam(b, e) -> Lam(b, mapVarsExpr f e)
    | Let(bs, e) -> Let(mapVarsBinds f bs, mapVarsExpr f e)
    | Case(e, b, alts) -> Case(mapVarsExpr f e, b, mapVarsAlts f alts)
    | App(a, b) -> App(mapVarsExpr f a, mapVarsExpr f b)
    | Prim ps -> Prim(ps |> List.map (mapVarsPrim f))
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

and mapVarsPat f = function
    |DataAlt v -> DataAlt (f v)
    |LitAlt l -> LitAlt l
    
and mapVarsPrim f =
    function
    | AVar v -> AVar (f v)
    | ALit l -> ALit l
    | AWasm w -> AWasm w

let mapVarsTopLevel f =
    function
    | TopExpr e -> TopExpr(mapVarsExpr f e)
    | TopConstr vs -> TopConstr(vs |> List.map f)

let mapVarsTopBind f (b, e) = (f b, mapVarsTopLevel f e)

let mapVarsProgram f p = p |> List.map (mapVarsTopBind f)

