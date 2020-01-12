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
    | Call of 'b * Atom<'b> list
    | Jump of 'b * Atom<'b> list
    | Prim of Atom<'b> list

and Binds<'b> =
    | Rec of 'b list
    | NonRec of 'b
    | Join of 'b * 'b list * Expr<'b>

and Bind<'b> = 'b * LambdaForm<'b>

and Default<'b> = Expr<'b>

and Alts<'b> =
    | AAlts of AAlts<'b> * Default<'b>
    | PAlts of PAlts<'b> * Default<'b>
and AAlts<'b> = ((Constr<'b> * 'b list) * Expr<'b>) list
and PAlts<'b> = (Wasm.Instr * Expr<'b>) list
and Lifted<'b> = Lifted of Bind<'b>
and Lets<'b> = Lifted<'b> list

and LambdaForm<'b> =
    { args: 'b list
      frees: 'b list
      locals: 'b list
      lets: ('b * LambdaForm<'b>) list
      expr: Expr<'b> }

type TopLevel<'b> =
    |TopCaf of LambdaForm<'b>
    |TopLam of LambdaForm<'b>
    |TopConstr of 'b list      

type TopBind<'b> = 'b * TopLevel<'b>
type Program<'b> = TopBind<'b> list


let lambdaForm e =
    { args = []
      frees = []
      locals = []
      lets = []
      expr = e }

let combineLf lf1 lf2 = 
    { args = [ lf1.args; lf2.args ] |> List.concat
      frees = [ lf1.frees; lf2.frees ] |> List.concat
      locals = [ lf1.locals; lf2.locals ] |> List.concat
      lets = [ lf1.lets; lf2.lets ] |> List.concat
      expr = lf1.expr }     

let normLf lf =
    { args = lf.args |> List.distinct
      frees = lf.frees |> List.distinct
      locals = lf.locals |> List.distinct
      lets = lf.lets |> List.distinct
      expr = lf.expr
    }
