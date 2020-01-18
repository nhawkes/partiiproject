module Vars

open Types

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

type CallType =
    | JoinCall
    | DirectCall
    | ConstrCall

[<StructuredFormatDisplay("{unique}${name} ")>]
type Var =
    { unique: Unique
      name: string
      typ: Typ
      callType: CallType option }

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
      callType = None }

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
