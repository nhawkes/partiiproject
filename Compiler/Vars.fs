module Vars

open Types



type Var = {v:Ast.Var option; typ:Types.Typ}
let intConstr = (Core.s2n "Int", {v=None; typ=FuncT(IntT, ValueT)})
let addOp = (Core.s2n "add", {v=None; typ=FuncT(ValueT, FuncT(ValueT, ValueT))})
let subOp = (Core.s2n "sub", {v=None; typ=FuncT(ValueT, FuncT(ValueT, ValueT))})

(*
type BuiltIn =
    | IntegerConstr
    | AddOp
    | SubOp

type Unique =
    | Export of string
    | User of int
    | Generated of int
    | Anonymous of int
    | BuiltIn of BuiltIn
    | Internal of string
    | InternalField of int
    | JoinPoint of int
    | Worker of Unique
    | Inline of int * Unique
    | Specialized of int * Unique

type CallType =
    | JoinCall
    | DirectCall
    | ConstrCall

let rec uniqueToStr name = function
      | Export s -> sprintf "E_%s" s
      | User i -> name
      | Generated i -> sprintf "gen_%i" i
      | Anonymous i -> sprintf "anon_%i" i
      | BuiltIn b -> sprintf "%A" b
      | Internal s ->  sprintf "intern_%s" s
      | InternalField i -> sprintf "intern_%i" i
      | JoinPoint i -> sprintf "join_%i" i
      | Worker u -> sprintf "work_%s" (uniqueToStr name u)
      | Inline (_, u) -> uniqueToStr name u
      | Specialized(_, u) -> sprintf "spec_%s" (uniqueToStr name u)

[<StructuredFormatDisplay("{AsString} ")>]
type Var =
    { unique: Unique
      name: string
      typ: Typ
      callType: CallType option }
    member m.AsString = 
      uniqueToStr m.name m.unique
let anonymousVar =
    let i = ref 0
    fun typ ->
        let next = !i
        i := !i + 1
        { unique = Anonymous next
          name = ""
          typ = typ
          callType = None }

let userVar =
    let i = ref 0
    fun name typ ->
        let next = !i
        i := !i + 1
        { unique = User next
          name = name
          typ = typ
          callType = None }

let generateVar =
    let i = ref 0
    fun typ ->
        let next = !i
        i := !i + 1
        { unique = Generated next
          name = ""
          typ = typ
          callType = None }


let generateJoin =
    let i = ref 0
    fun typ ->
        let next = !i
        i := !i + 1
        { unique = JoinPoint next
          name = ""
          typ = typ
          callType = Some JoinCall }

let exportVar s typ =
    { unique = Export s
      name = s
      typ = typ
      callType = Some DirectCall }

let inlineVar = 
    let i = ref 0
    fun () -> 
        let next = !i
        i := !i + 1
        fun var -> {var with unique=Inline(next, var.unique)}

let specializeVar = 
    let i = ref 0
    fun () -> 
        let next = !i
        i := !i + 1
        fun var -> {var with unique=Specialized(next, var.unique)}


let integerConstr =
    { unique = BuiltIn IntegerConstr
      name = "Int"
      typ = createFuncT SatFunc [ ValueT ] ValueT
      callType = Some ConstrCall }

let addOp =
    { unique = BuiltIn AddOp
      name = ""
      typ = createFuncT SatFunc [ ValueT; ValueT ] ValueT
      callType = Some DirectCall }

let subOp =
    { unique = BuiltIn SubOp
      name = ""
      typ = createFuncT SatFunc [ ValueT; ValueT ] ValueT
      callType = Some DirectCall }

let xInt =
    { unique = Internal "xInt"
      name = ""
      typ = IntT
      callType = None }

let yInt =
    { unique = Internal "yInt"
      name = ""
      typ = IntT
      callType = None }

let rInt =
    { unique = Internal "rInt"
      name = ""
      typ = IntT
      callType = None }

let xValue =
    { unique = Internal "xValue"
      name = ""
      typ = ValueT
      callType = None }

let yValue =
    { unique = Internal "yValue"
      name = ""
      typ = ValueT
      callType = None }

let rValue =
    { unique = Internal "rValue"
      name = ""
      typ = ValueT
      callType = None }


*)