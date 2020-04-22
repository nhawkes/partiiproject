module Analysis

open Vars
open Types
open Core
open Xunit
open Analysis
let inline b<'a> s :Binder<unit> = s2n s, ()


[<Fact>]
let StrictnessAnalysis() =
    let if1Expr = Core.caseE(Core.varS (s2n "a"),b "_", ([
        Core.Lit(Core.I32 0), [], Core.varS (s2n "b")
        Core.Lit(Core.I32 1), [], Core.varS (s2n "c")
    ]))
    let if1ExprAnalysis, _ = Analysis.analyseExpr Map.empty 0 if1Expr
    let if2Expr = Core.caseE(Core.varS (s2n "a"),b "_", ([
        Core.Lit(Core.I32 0), [], Core.varS (s2n "b")
    ]))
    let if2ExprAnalysis, _ = Analysis.analyseExpr Map.empty 0 if2Expr
    let app1Expr = Core.caseE(Core.varS (s2n "a"),b "_", ([
        Core.Lit(Core.I32 0), [], Core.App(Core.varS (s2n "f"), Core.varS (s2n "b"))        
    ]))
    let app1ExprAnalysis, _ = Analysis.analyseExpr Map.empty 0 app1Expr
    let app2Expr = Core.App(Core.lamE (b "b", Core.varS (s2n "b")), Core.varS (s2n "a"))
    let app2ExprAnalysis, _ = Analysis.analyseExpr Map.empty 0 app2Expr
    let letExpr = Core.letE(Core.NonRec, [b "f", Core.lamE (b "b", Core.varS (s2n "b"))], Core.App(Core.varS (s2n "f"), Core.varS (s2n "a")))
    let letExprAnalysis, _ = Analysis.analyseExpr Map.empty 0 letExpr
    let bottomExpr = Core.letE(Core.Rec, [b "f", Core.lamE (b "b", Core.App (Core.varS (s2n "f"), Core.varS (s2n "b")))], Core.App(Core.varS (s2n "f"), Core.varS (s2n "a")))
    let bottomExprAnalysis, _ = Analysis.analyseExpr Map.empty 0 bottomExpr
    let letRecExpr = 
        Core.letE(
            Core.Rec, [
                b "f", Core.lamE (b "b", 
                    Core.caseE(Core.varS (s2n "b"),b "_", ([
                        Core.Lit(Core.I32 0), [], Core.varS (s2n "c") 
                        Core.Lit(Core.I32 1), [], Core.App(Core.varS (s2n "f"), Core.varS (s2n "b")) 
                    ])))
            ], Core.App(Core.varS (s2n "f"), Core.varS (s2n "a")))
    let letRecExprAnalysis, _ = Analysis.analyseExpr Map.empty 0 letRecExpr
    printfn "%A" if1Expr
    printfn "%A" if1ExprAnalysis
    Assert.Equal({ args = TopArgStrictness; frees = (Lazy, Map.ofList [(s2n "a", Strict 0)]) }, if1ExprAnalysis)
    printfn "%A" if2Expr
    printfn "%A" if2ExprAnalysis
    Assert.Equal({ args = TopArgStrictness; frees = (Lazy, Map.ofList [(s2n "a", Strict 0); (s2n "b", Strict 0)]) }, if2ExprAnalysis)
    printfn "%A" app1Expr
    printfn "%A" app1ExprAnalysis
    Assert.Equal({ args = TopArgStrictness; frees = (Lazy, Map.ofList [(s2n "a", Strict 0); (s2n "f", Strict 1)]) }, app1ExprAnalysis)
    printfn "%A" app2Expr
    printfn "%A" app2ExprAnalysis
    Assert.Equal({ args = TopArgStrictness; frees = (Lazy, Map.ofList [(s2n "a", Strict 0)]) }, app2ExprAnalysis)
    printfn "%A" letExpr
    printfn "%A" letExprAnalysis
    Assert.Equal({ args = TopArgStrictness; frees = (Lazy, Map.ofList [(s2n "a", Strict 0)]) }, letExprAnalysis)
    printfn "%A" bottomExpr
    printfn "%A" bottomExprAnalysis
    Assert.Equal({ args = BottomArgStrictness; frees = (HyperStrict, Map.ofList []) }, bottomExprAnalysis)
    printfn "%A" letRecExpr
    printfn "%A" letRecExprAnalysis
    Assert.Equal({ args = TopArgStrictness; frees = (Lazy, Map.ofList [(s2n "a", Strict 0); (s2n "c", Strict 0)]) }, letRecExprAnalysis)
