module Stg

type Constr = Constr of int
type Var = Var of int

type Atom =
    |Var of Var
    |Literal of Wasm.Instr

type Expr =
    |Let of Binds * Expr
    |Case of Bind * Alts * Expr
    |Var of Var * Atom list
    |Constr of Constr * Atom list
    |Prim of Wasm.Instr * Atom list
and Binds =
    |Rec of Bind list
    |NonRec of Bind list
and Bind = Var * Expr
and Alts =
    |AAlts of ((Constr * Var list) * Expr) list
    |PAlts of (Wasm.Instr * Expr) list
and LambdaForm =
    Args * Free * Expr
and Args = Var list
and Free = Var list
and Program = Bind list        



