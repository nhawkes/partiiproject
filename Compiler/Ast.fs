module Ast

type Boxed =
    | Integer of int32

type Lit = 
    | Raw of Core.Lit
    | Box of Boxed

type Pattern<'b> =
    | PatLit of Lit
    | PatBind of 'b
    | PatConstr of 'b * Pattern<'b> list

type Expr<'b> =
    | Lit of Lit
    | Var of 'b
    | App of 'b * Expr<'b> list
    | Match of Expr<'b> * Case<'b> list
    | Block of Block<'b>
    | Prim of Prim<'b> list

and Prim<'b> =
    | PrimVar of 'b
    | PrimWasm of Wasm.Instr    

and Case<'b> = Pattern<'b> * Expr<'b>

and Block<'b> = 'b list * Statement<'b> list    

and Statement<'b> =
    | Assign of 'b * Expr<'b>
    | Return of Expr<'b>    

type Declaration<'b> =
    | GlobalDecl of 'b * Expr<'b>
    | TypeDecl of 'b * 'b list

type Program<'b> =
    Declaration<'b> list