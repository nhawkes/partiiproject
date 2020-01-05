module Vars
open Ast

type BuiltIn =
    | IntegerConstr
    | AddOp
    | SubOp

type Unique =
    |Export of string
    |User of int
    |Generated of int
    |Anonymous of int
    |BuiltIn of BuiltIn
    |Internal of string
    |InternalField of int

type Var = {unique:Unique; name:string; typ:Typ}

let anonymousVar =
    let i = ref 0
    fun typ -> 
        let next = !i
        i := !i+1
        {unique=Anonymous next; name=""; typ=typ}

let userVar name =
    let i = ref 0
    fun typ -> 
        let next = !i
        i := !i+1
        {unique=User next; name=name; typ=typ}        

let generateVar =
    let i = ref 0
    fun typ -> 
        let next = !i
        i := !i+1
        {unique=Generated next; name=""; typ=typ}
        

let exportVar s typ = {unique=Export s; name=s; typ=typ}  

let integerConstr = 
    {unique=BuiltIn IntegerConstr;  name="Int"; typ=TopFuncT([ValueT], ValueT)}
let addOp = 
    {unique=BuiltIn AddOp;  name=""; typ=TopFuncT([ValueT; ValueT], ValueT)}
let subOp = 
    {unique=BuiltIn SubOp;  name=""; typ=TopFuncT([ValueT; ValueT], ValueT)}

let xInt = 
    {unique=Internal "xInt";  name=""; typ=IntT}
let yInt = 
    {unique=Internal "yInt";  name=""; typ=IntT}
let rInt = 
    {unique=Internal "rInt";  name=""; typ=IntT}
let xValue = 
    {unique=Internal "xValue";  name=""; typ=ValueT}
let yValue = 
    {unique=Internal "yValue";  name=""; typ=ValueT}
let rValue = 
    {unique=Internal "rValue";  name=""; typ=ValueT}
