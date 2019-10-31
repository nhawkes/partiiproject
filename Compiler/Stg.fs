module Stg

type Constr = int

type Var = string

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
    | Constr of Constr * Atom list
    | Prim of Atom list

and Binds =
    | Rec of Bind list
    | NonRec of Bind list

and Bind = Var * LambdaForm

and Default = Expr

and Alts =
    | AAlts of AAlts * Default
    | PAlts of PAlts * Default
and AAlts = ((Constr * Var list) * Expr) list
and PAlts = (Wasm.Instr * Expr) list
and LambdaForm = (Args * Free * Locals) * Expr

type Program = Bind list
