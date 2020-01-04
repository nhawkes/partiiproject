module Ast

type Typ =
    |TopFuncT of Typ list * Typ
    |FuncT of Typ * Typ
    |IntT    
    |ValueT

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
    | Match of (Expr<'b> * Typ) * Case<'b> list
    | Block of Block<'b>
    | Prim of Prim<'b> list

and Prim<'b> =
    | PrimVar of 'b
    | PrimWasm of Wasm.Instr    

and Case<'b> = Pattern<'b> * Expr<'b>

and Block<'b> = Statement<'b> list    

and Statement<'b> =
    | Assign of 'b * 'b list * Expr<'b>
    | Return of Expr<'b>    

type Declaration<'b> =
    | ExportDecl of string * string list * Expr<'b>
    | GlobalDecl of 'b * 'b list * Expr<'b>
    | TypeDecl of 'b * 'b list

type Program<'b> =
    Declaration<'b> list

type BuiltIn =
    | IntegerConstr

    
    
type Unique =
    |Export of string
    |Global of string
    |Local of int
    |BuiltIn of BuiltIn
type Var = {unique:Unique; name:string; typ:Typ}

let typeofBuiltIn = function
    | IntegerConstr -> TopFuncT([IntT], ValueT)

let freshVar =
    let i = ref 0
    fun typ -> 
        let next = !i
        i := !i+1
        {unique=Local next; name=""; typ=typ}
let localVar  = 
    let map = ref Map.empty
    fun s typ ->
    match !map |> Map.tryFind s with
    |Some value -> value
    |None ->
        let newVar = {freshVar typ with name=s}     
        map := !map |> Map.add s newVar
        newVar

let globalVar s typ = {unique=Global s; name=s; typ=typ}  
let exportVar s typ = {unique=Export s; name=s; typ=typ}  
let builtInVar b = {unique=BuiltIn b; name=sprintf "%A" b; typ=typeofBuiltIn b}