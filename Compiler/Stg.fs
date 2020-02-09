module Stg

type Constr = int
[<StructuredFormatDisplay("{name}.{unique}")>]
type Var = {name:string; info:System.IComparable; unique:int; prim:bool}
type Args = Var list
type Free = Var list
type Locals = Var list

type Atom =
    | AVar of Var
    | ALit of Wasm.Instr

type Expr =
    | Let of Binds * Expr
    | Case of Expr * Var * Alts
    | App of Var * Atom list
    | Call of Var * Atom list
    | Constr of Var * Atom list
    | Jump of Var * Atom list
    | Prim of Atom list

and Binds =
    | Rec of Var list
    | NonRec of Var
    | Join of Var * Var list * Expr

and Bind = Var * LambdaForm

and Default = Expr

and Alts =
    | AAlts of AAlts * Default
    | PAlts of PAlts * Default
and AAlts = ((Constr * Var list) * Expr) list
and PAlts = (Wasm.Instr * Expr) list
and Lifted = Lifted of Bind
and Lets = Lifted list

and LambdaForm =
    { args: Var list
      frees: Var list
      locals: Var list
      lets: (Var * LambdaForm) list
      stdConstrs: (Var * Var * Atom list) list
      expr: Expr }

type TopLevel =
    |TopCaf of LambdaForm
    |TopLam of Args * LambdaForm
    |TopExport of string * Args * LambdaForm
    |TopConstr of Constr * Var list

type TopBind = Var * TopLevel
type Program = TopBind list


let lambdaForm e =
    { args = []
      frees = []
      locals = []
      lets = []
      stdConstrs = []
      expr = e }

let combineLf lf1 lf2 = 
    { args = [ lf1.args; lf2.args ] |> List.concat
      frees = [ lf1.frees; lf2.frees ] |> List.concat
      locals = [ lf1.locals; lf2.locals ] |> List.concat
      lets = [ lf1.lets; lf2.lets ] |> List.concat
      stdConstrs = [ lf1.stdConstrs; lf2.stdConstrs ] |> List.concat
      expr = lf1.expr }     

let normLf lf =
    { args = lf.args |> List.distinct
      frees = lf.frees |> List.distinct
      locals = lf.locals |> List.distinct
      lets = lf.lets |> List.distinct
      stdConstrs = lf.stdConstrs |> List.distinct
      expr = lf.expr
    }
