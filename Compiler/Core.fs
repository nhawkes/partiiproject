module Core

type Lit = I32 of int32

type Expr<'b> =
    | Var of Vars.Var
    | Lit of Lit
    | Lam of 'b * Expr<'b>
    | Let of Binds<'b> * Expr<'b>
    | Case of Expr<'b> * 'b * Alts<'b>
    | App of Expr<'b> * Expr<'b>
    | Prim of Stg.Atom<'b> list

and Binds<'b> =
    | Rec of Bind<'b> list
    | NonRec of Bind<'b> list
    | Join of Bind<'b>


and Bind<'b> = 'b * Expr<'b>


and Default<'b> = Expr<'b>


and Alts<'b> = ((Expr<'b> * 'b list) * Expr<'b>) list * Default<'b>

type TopLevel<'b> =
    | TopExpr of Expr<'b>
    | TopConstr of 'b list

type TopBind<'b> = 'b * TopLevel<'b>

type Program<'b> = TopBind<'b> list



let rec mapExpr f = function
    | Var v -> Var (f v) 
    | Lit l -> Lit l
    | Lam (v, e) -> Lam(f v, mapExpr f e)
    | Let (bs, e) -> Let(mapBinds f bs, mapExpr f e) 
    | Case(e, b, alts) -> Case(mapExpr f e, f b, mapAlts f alts)
    | App (a, b) -> App(mapExpr f a, mapExpr f b)
    | Prim ps -> Prim(ps |> List.map (mapPrim f)) 

and mapBinds f = function
    | Rec bs -> Rec(bs |> List.map(fun (b, e) -> (f b, mapExpr f e)))
    | NonRec bs -> Rec(bs |> List.map(fun (b, e) -> (f b, mapExpr f e)))
    | Join (b, e) -> Join(f b, mapExpr f e)


and mapAlts f = function
    |(cases, def) -> cases |> List.map (mapAlt f), mapExpr f def

and mapAlt f = function
    |(ev, vs), e -> (mapExpr f ev, vs |> List.map f), mapExpr f e
           
and mapPrim f = function
    | Stg.AVar v -> Stg.AVar (f v)
    | Stg.ALit l -> Stg.ALit l

let mapTopLevel f = function
    | TopExpr e -> TopExpr (mapExpr f e)
    | TopConstr vs -> TopConstr(vs |> List.map f)

let mapTopBind f (b, e) = (f b, mapTopLevel f e) 

let mapProgram f p = p |> List.map (mapTopBind f)