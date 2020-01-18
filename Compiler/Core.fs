module Core

type Lit = I32 of int32

type Expr<'v, 'b> =
    | Var of 'v
    | Lit of Lit
    | Lam of 'b * Expr<'v, 'b>
    | Let of Binds<'v, 'b> * Expr<'v, 'b>
    | Case of Expr<'v, 'b> * 'b * Alts<'v, 'b>
    | App of Expr<'v, 'b> * Expr<'v, 'b>
    | Prim of Stg.Atom<'v> list
    | Unreachable

and Binds<'v, 'b> =
    | Rec of Bind<'v, 'b> list
    | NonRec of Bind<'v, 'b> list
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



let rec mapExpr (f:'a->'b) : Expr<'v, 'a> -> Expr<'v, 'b> =
    function
    | Var v -> Var(v)
    | Lit l -> Lit l
    | Lam(v, e) -> Lam(f v, mapExpr f e)
    | Let(bs, e) -> Let(mapBinds f bs, mapExpr f e)
    | Case(e, b, alts) -> Case(mapExpr f e, f b, mapAlts f alts)
    | App(a, b) -> App(mapExpr f a, mapExpr f b)
    | Prim ps -> Prim(ps |> List.map (mapPrim f))

and mapBinds f =
    function
    | Rec bs -> Rec(bs |> List.map (fun (b, e) -> (f b, mapExpr f e)))
    | NonRec bs -> Rec(bs |> List.map (fun (b, e) -> (f b, mapExpr f e)))
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
    | Stg.AVar v -> Stg.AVar(v)
    | Stg.ALit l -> Stg.ALit l

let mapTopLevel f =
    function
    | TopExpr e -> TopExpr(mapExpr f e)
    | TopConstr vs -> TopConstr(vs |> List.map f)

let mapTopBind f (b, e) = (f b, mapTopLevel f e)

let mapProgram f p = p |> List.map (mapTopBind f)
