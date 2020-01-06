module Renamer

open Ast
open BuiltIns
open Vars
open Types


let lookup env b =
    match b with
    |"" -> failwithf "Cannot lookup empty"
    |"_" -> failwithf "'_' can only be used in bindings"
    |_ ->
    match env |> Map.tryFind (b) with
    |Some v -> v
    |None -> failwithf "%s is not defined" b
    
let newVar typ = function    
    |"_" -> anonymousVar typ
    |b -> userVar b typ    

let put env (var:Var) =
    env |> Map.add (var.name) var

let rec putAll env = function
    |v::vs -> putAll (put env v) vs
    |[] -> env    

let rec renamePattern env = function
    | PatLit l -> PatLit l, env
    | PatBind b -> 
        let v = newVar ValueT b
        PatBind (v), put env v
    | PatConstr(b, bs) ->
        let v = lookup env b
        let vs, newEnv = renamePatterns env [] bs
        PatConstr(v, vs), newEnv

and renamePatterns env ys = function
    |x::xs ->
        let y, newEnv = renamePattern env x
        renamePatterns newEnv (y::ys) xs
    |[] ->
        ys |> List.rev, env    

let rec renameVars env vs = function
    |b::bs ->        
        let v = newVar ValueT b
        renameVars (put env v) (v::vs) bs
    |[] ->
        vs |> List.rev, env        

let rec renameExpr env = function
    | Lit l -> Lit l
    | Var v -> Var (lookup env v)
    | BinOp(a, o, b) -> BinOp(renameExpr env a, o, renameExpr env b)
    | Call(b, es) -> 
        Call(lookup env b, es |> List.map (renameExpr env))
    | Match((e,t), cases) ->
        Match((renameExpr env e, t), renameCases env cases)
    | Block(block) -> renameBlock env [] None block
    | Prim ps -> ps |> List.map (renamePrim env) |> Prim

and renamePrim env = function
    | PrimVar b -> PrimVar(lookup env b)
    | PrimWasm w -> PrimWasm w

and renameCase env (pattern, e) =
    let pat, newEnv = renamePattern env pattern
    (pat, renameExpr newEnv e)

and renameCases env = List.map (renameCase env)

and renameBlock env assigns ret = function
    |Assign(b, bs, e)::xs -> 
        let typ =  createFuncT(ValueT |> List.replicate (bs |> List.length)) (ValueT)
        let v = newVar typ b
        let vs = bs |> List.map (newVar ValueT)
        let newEnv = put env v
        renameBlock newEnv ((v, vs, e)::assigns) ret xs
    |Return(e)::xs ->
        renameBlock env assigns (ret |> Option.orElse (Some e)) xs
    |[] -> 
        let assignsRenamed = assigns |> List.map (renameAssign env)
        let retRenamed = ret |> Option.toList |> List.map(renameReturn env)
        Block(List.concat [assignsRenamed; retRenamed])

and renameAssign env (v, vs, e) =
    let newEnv = putAll env vs
    Assign(v, vs, renameExpr newEnv e)

and renameReturn env e = Return(renameExpr env e)



let rec renameDecls env exportDecls globalDecls typeDecls = function
    | ExportDecl(n, (b, bs), e)::xs -> 
        let typ =  createFuncT(ValueT |> List.replicate (bs |> List.length)) (ValueT)
        let callArity = bs |> List.length
        let v = {newVar typ b with callArity=Some callArity}
        let vs = bs |> List.map (newVar ValueT)
        let newEnv = put env v
        renameDecls newEnv ((n, (v,vs), e)::exportDecls) globalDecls typeDecls xs
    | GlobalDecl(b, bs, e)::xs ->
        let typ =  createFuncT(ValueT |> List.replicate (bs |> List.length)) (ValueT)
        let callArity = bs |> List.length
        let v = {newVar typ b with callArity=Some callArity}
        let vs = bs |> List.map (newVar ValueT)
        let newEnv = put env v
        renameDecls newEnv exportDecls ((v, vs, e)::globalDecls) typeDecls xs
    | TypeDecl(b, bs)::xs -> 
        let typ =  createFuncT(ValueT |> List.replicate (bs |> List.length)) (ValueT)
        let callArity = bs |> List.length
        let v = {newVar typ b with callArity=Some callArity}
        let vs = bs |> List.map (newVar ValueT)
        let newEnv = put env v
        renameDecls newEnv exportDecls globalDecls ((v,vs)::typeDecls) xs

    |[] ->
       let exportDeclsRenamed = exportDecls |> List.map (renameExportDecl env)
       let globalDeclsRenamed = globalDecls |> List.map (renameGlobalDecl env)
       let typeDeclsRenamed = typeDecls |> List.map (renameTypeDecl env)
       List.concat [
           exportDeclsRenamed
           globalDeclsRenamed
           typeDeclsRenamed           
       ]

and renameExportDecl env (n, (v,vs), e) =
    let newEnv = putAll env vs
    ExportDecl(n, (v, vs), renameExpr newEnv e)

and renameGlobalDecl env (v, vs, e) =
    let newEnv = putAll env vs
    GlobalDecl(v, vs, renameExpr newEnv e)

and renameTypeDecl env (v, vs) = 
    TypeDecl(v, vs)


let renameProgram program =
    let env = BuiltIns.builtInsEnv
    List.concat[
        BuiltIns.builtIns
        renameDecls env [] [] [] program
    ]
    
