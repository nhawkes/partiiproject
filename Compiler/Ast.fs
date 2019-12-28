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
    | Call of 'b * Expr<'b> list
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

type BuiltIn =
    | IntegerConstr
    
    
type Unique =
    |Global of string
    |Local of int
    |BuiltIn of BuiltIn
type Var = {unique:Unique; name:string}


let freshVar =
    let i = ref 0
    fun () -> 
        let next = !i
        i := !i+1
        {unique=Local next; name=""}
let localVar = 
    let map = ref Map.empty
    fun s ->
    match !map |> Map.tryFind s with
    |Some value -> value
    |None ->
        let newVar = {freshVar() with name=s}     
        map := !map |> Map.add s newVar
        newVar

let globalVar s = {unique=Global s; name=s }  
let builtInVar b = {unique=BuiltIn b; name=sprintf "%A" b}