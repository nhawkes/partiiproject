module Vars

open Types



[<StructuredFormatDisplay("{v} ")>]
type Var = {v:Ast.Var option; hintInline:bool; typ:Types.Typ}
let intConstr = (Core.s2n "Int", {v=None; hintInline=true; typ=FuncT(IntT, ValueT)})
let boolConstr = (Core.s2n "Bool", {v=None; hintInline=true; typ=FuncT(IntT, ValueT)})
let addOp = (Core.s2n "add", {v=None; hintInline=true; typ=FuncT(ValueT, FuncT(ValueT, ValueT))})
let subOp = (Core.s2n "sub", {v=None; hintInline=true; typ=FuncT(ValueT, FuncT(ValueT, ValueT))})
let mulOp = (Core.s2n "mul", {v=None; hintInline=true; typ=FuncT(ValueT, FuncT(ValueT, ValueT))})
let divOp = (Core.s2n "div", {v=None; hintInline=true; typ=FuncT(ValueT, FuncT(ValueT, ValueT))})
let equalsOp = (Core.s2n "equals", {v=None; hintInline=true; typ=FuncT(ValueT, FuncT(ValueT, ValueT))})
let lessThanOp = (Core.s2n "lessThan", {v=None; hintInline=true; typ=FuncT(ValueT, FuncT(ValueT, ValueT))})
let trueConst = (Core.s2n "true", {v=None; hintInline=true; typ=ValueT})
let falseConst = (Core.s2n "false", {v=None; hintInline=true; typ=ValueT})
