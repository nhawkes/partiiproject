module Ast

[<StructuredFormatDisplay("{AsString}")>]
type Var = 
    {text:string; startPos: FParsec.Position; endPos: FParsec.Position}
    member x.AsString = x.text 
        

type Boxed = Integer of int32

type Lit =
    | Raw of Core.Lit
    | Box of Boxed

type Pattern =
    | PatLit of Lit
    | PatBind of Var
    | PatConstr of Var * Pattern list

type Op =
    | Add
    | Sub
    | Equals
    | LessThan
type Expr =
    | Lit of Lit
    | Var of Var
    | BinOp of Expr * Op * Expr
    | Call of Var * Expr list
    | Match of (Expr * Types.Typ) * Case list
    | If of Expr * (Expr * Expr)
    | Block of Block
    | Prim of Wasm.Instr * Var list


and Prim =
    | PrimVar of Var
    | PrimWasm of Wasm.Instr

and Case = Pattern * Expr

and Block = Statement list

and Statement =
    | Assign of AssignLHS * Expr
    | Return of Expr

and AssignLHS =
    |AssignVar of Var
    |AssignFunc of Var * AssignLHS list

type Declaration =
    | ExportDecl of (string * string list) * (Var * Var list) * Expr
    | GlobalDecl of AssignLHS * Expr
    | TypeDecl of AssignLHS

type Program = Declaration list

