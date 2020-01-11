module Core

type Lit = I32 of int32

type Expr<'b> =
    | Var of 'b
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
