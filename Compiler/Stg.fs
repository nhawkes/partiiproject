module Stg

type Constr<'b> = 'b

type Args<'b> = 'b list
type Free<'b> = 'b list
type Locals<'b> = 'b list

type Atom<'b> =
    | AVar of 'b
    | ALit of Wasm.Instr

type Expr<'b> =
    | Let of Binds<'b> * Expr<'b>
    | Case of Expr<'b> * 'b * Alts<'b>
    | App of 'b * Atom<'b> list
    | Constr of Constr<'b> * Atom<'b> list
    | Prim of Atom<'b> list

and Binds<'b> =
    | Rec of Bind<'b> list
    | NonRec of Bind<'b> list

and Bind<'b> = 'b * LambdaForm<'b>

and Default<'b> = Expr<'b>

and Alts<'b> =
    | AAlts of AAlts<'b> * Default<'b>
    | PAlts of PAlts<'b> * Default<'b>
and AAlts<'b> = ((Constr<'b> * 'b list) * Expr<'b>) list
and PAlts<'b> = (Wasm.Instr * Expr<'b>) list
and LambdaForm<'b> = (Args<'b> * Free<'b> * Locals<'b>) * Expr<'b>

type TopLevel<'b> =
    |TopLam of LambdaForm<'b>
    |TopConstr of 'b list

type TopBind<'b> = 'b * TopLevel<'b>
type Program<'b> = TopBind<'b> list
