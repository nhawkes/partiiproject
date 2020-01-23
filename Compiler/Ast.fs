module Ast



type Boxed = Integer of int32

type Lit =
    | Raw of Core.Lit
    | Box of Boxed

type Pattern<'b> =
    | PatLit of Lit
    | PatBind of 'b
    | PatConstr of 'b * Pattern<'b> list

type Op =
    | Add
    | Sub

type Expr<'b> =
    | Lit of Lit
    | Var of 'b
    | BinOp of Expr<'b> * Op * Expr<'b>
    | Call of 'b * Expr<'b> list
    | Match of (Expr<'b> * Types.Typ) * Case<'b> list
    | Block of Block<'b>
    | Prim of Wasm.Instr * 'b list

and Prim<'b> =
    | PrimVar of 'b
    | PrimWasm of Wasm.Instr

and Case<'b> = Pattern<'b> * Expr<'b>

and Block<'b> = Statement<'b> list

and Statement<'b> =
    | Assign of AssignLHS<'b> * Expr<'b>
    | Return of Expr<'b>

and AssignLHS<'b> =
    |AssignVar of 'b
    |AssignFunc of 'b * AssignLHS<'b> list

type Declaration<'b> =
    | ExportDecl of (string * string list) * ('b * 'b list) * Expr<'b>
    | GlobalDecl of AssignLHS<'b> * Expr<'b>
    | TypeDecl of AssignLHS<'b>

type Program<'b> = Declaration<'b> list
