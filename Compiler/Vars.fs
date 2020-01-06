module Vars
open Ast
open Types

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

type Var = {unique:Unique; name:string; typ:Typ; callArity: int option}

let anonymousVar =
    let i = ref 0
    fun typ -> 
        let next = !i
        i := !i+1
        {unique=Anonymous next; name=""; typ=typ; callArity=None}

let userVar name =
    let i = ref 0
    fun typ -> 
        let next = !i
        i := !i+1
        {unique=User next; name=name; typ=typ; callArity=None}        

let generateVar =
    let i = ref 0
    fun typ -> 
        let next = !i
        i := !i+1
        {unique=Generated next; name=""; typ=typ; callArity=None}
        

let exportVar s typ = {unique=Export s; name=s; typ=typ; callArity=None}  

let integerConstr = 
    {unique=BuiltIn IntegerConstr;  name="Int"; typ=FuncT(ValueT, ValueT); callArity=Some 1}
let addOp = 
    {unique=BuiltIn AddOp;  name=""; typ=FuncT(ValueT, FuncT(ValueT, ValueT)); callArity=Some 2}
let subOp = 
    {unique=BuiltIn SubOp;  name=""; typ=FuncT(ValueT, FuncT(ValueT, ValueT)); callArity=Some 2}

let xInt = 
    {unique=Internal "xInt";  name=""; typ=IntT; callArity=None}
let yInt = 
    {unique=Internal "yInt";  name=""; typ=IntT; callArity=None}
let rInt = 
    {unique=Internal "rInt";  name=""; typ=IntT; callArity=None}
let xValue = 
    {unique=Internal "xValue";  name=""; typ=ValueT; callArity=None}
let yValue = 
    {unique=Internal "yValue";  name=""; typ=ValueT; callArity=None}
let rValue = 
    {unique=Internal "rValue";  name=""; typ=ValueT; callArity=None}
