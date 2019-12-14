module Tests

open System
open Xunit
open Emit
open Wasm
open Stg
open WasmGen

let arrayFromTuple =
    function
    | null -> [||]
    | t when Reflection.FSharpType.IsTuple(t.GetType()) -> Reflection.FSharpValue.GetTupleFields t
    | x -> [| x |]

let (?) (this: 'Source) (prop: string): obj -> 'Result =
    let p = this.GetType().GetMethod(prop)
    fun x -> p.Invoke(this, arrayFromTuple x) :?> 'Result

let compile byteList =
    let bytes = byteList |> List.toArray
    use stream = new IO.MemoryStream(bytes)
    WebAssembly.Runtime.Compile.FromBinary(stream).Invoke(Collections.Generic.Dictionary<_, _>())


[<Fact>]
let ``WASM Fibinacci 7``() =
    let wasmModule =
        [ TypeSec [ [ I32 ], [ I32 ] ]
          FuncSec [ 0u ]
          ExportSec
              [ { nm = "Fibonacci"
                  exportdesc = ExportFunc 0u } ]
          CodeSec
              [ [],
                [ LocalGet 0u
                  I32Const 0
                  I32Eq
                  IfElse
                      ([ I32 ], [ I32Const 1 ],
                       [ LocalGet 0u
                         I32Const 1
                         I32Eq
                         IfElse
                             ([ I32 ], [ I32Const 1 ],
                              [ LocalGet 0u
                                I32Const 1
                                I32Sub
                                Call 0u

                                LocalGet 0u
                                I32Const 2
                                I32Sub
                                Call 0u

                                I32Add ]) ]) ] ] ]
    let fibonacciProgram = emitWasmModule wasmModule |> compile
    let output = fibonacciProgram.Exports?Fibonacci (7)
    Assert.Equal(21, output)


[<Fact>]
let Malloc() =
    let wasmModule =
        [ TypeSec [ WasmGen.malloc.functype ]
          FuncSec [ 0u ]
          MemSec [ Min 1u ]
          GlobalSec
              [ { gt = I32, Var
                  init = [ I32Const 0 ] } ]
          ExportSec
              [ { nm = "Malloc"
                  exportdesc = ExportFunc 0u }
                { nm = "Memory"
                  exportdesc = ExportMem 0u } ]
          CodeSec [ WasmGen.malloc.func ] ]

    let mallocProgram = emitWasmModule wasmModule |> compile
    let memory = mallocProgram.Exports?Memory
    let output1 = mallocProgram.Exports?Malloc (12)
    let output2 = mallocProgram.Exports?Malloc (12)
    Assert.Equal(20, output2)

[<Fact>]
let ``Boxed Int``() =
    let stgModule =
        [ (UserVar "Int", TopConstr [ UserVar "I" ])
          (UserVar "Get",
           TopLam
               (([ UserVar "i" ], [], [], []),
                Prim
                    [ ALit
                        (Wasm.I32Load
                            { align = 0u
                              offset = 0u })
                      ALit(Wasm.LocalGet 0u) ])) ]
    let boxedIntProgram =
        stgModule
        |> WasmGen.genProgram
        |> emitWasmModule
        |> compile

    let (output: Int32) = boxedIntProgram.Exports?Int (7)
    let memory = [ 0 .. 8 ] |> List.map ((*) 4 >> boxedIntProgram.Exports?Get)
    Assert.StrictEqual("[12; 0; 2; 7; 0; 0; 0; 0; 0]", sprintf "%A" memory)


[<Fact>]
let ``STG Fibinacci 7``() =
    let stgModule: Program<Var> =
        [ (UserVar "Fibonacci"),
          TopLam
              (([ (UserVar "x") ], [],
                [ (UserVar "a")
                  (UserVar "b") ], []),
               Case
                   (Prim [ AVar(UserVar "x") ], (UserVar "x"),
                    PAlts
                        ([ I32Const 0, Prim [ ALit(I32Const 1) ] ],
                         Case
                             (Prim [ AVar(UserVar "x") ], (UserVar "x"),
                              PAlts
                                  ([ I32Const 1, Prim [ ALit(I32Const 1) ] ],
                                   Case
                                       (Prim
                                           [ AVar(UserVar "Fibonacci")
                                             ALit I32Sub
                                             ALit(I32Const 1)
                                             AVar(UserVar "x") ], (UserVar "a"),
                                        PAlts
                                            ([],
                                             Case
                                                 (Prim
                                                     [ AVar(UserVar "Fibonacci")
                                                       ALit(I32Sub)
                                                       ALit(I32Const 2)
                                                       AVar((UserVar "x")) ], (UserVar "b"),
                                                  PAlts
                                                      ([],
                                                       Prim
                                                           [ ALit I32Add
                                                             AVar(UserVar "a")
                                                             AVar(UserVar "b") ]))))))))) ]
    let fibonacciProgram =
        stgModule
        |> WasmGen.genProgram
        |> emitWasmModule
        |> compile

    let output = fibonacciProgram.Exports?Fibonacci (7)
    Assert.Equal(21, output)


[<Fact>]
let ``STG Boxed Fibinacci 7``() =    
    let stgModule : Program<Var> =
        [
            (UserVar "Int", TopConstr [UserVar "I"])
            (UserVar "add"), TopLam (([(UserVar "x_boxed");(UserVar "y_boxed")],[],[(UserVar "x");(UserVar "y");(UserVar "r")],[]),            
                Case(
                    App((UserVar "x_boxed"), []),
                    (UserVar "x_boxed"),
                    AAlts (                       
                        [  (UserVar "Int", [(UserVar "x")]),
                            Case(
                                App((UserVar "y_boxed"), []),
                                (UserVar "x_boxed"),
                                AAlts (
                                    [(UserVar "Int",[(UserVar "y")]),
                                        Case(
                                            Prim [ALit Wasm.I32Add; AVar (UserVar "x"); AVar (UserVar "y")],
                                            (UserVar "r"),
                                            PAlts([], 
                                                Constr (UserVar "Int", [AVar (UserVar "r")])))                                    
                                    ],
                                    Prim [ALit Wasm.Unreachable; ALit (I32Const 0)]
                                )
                            )
                        ],
                        Prim [ALit Wasm.Unreachable; ALit (I32Const 0)]
                    )
                ) 
            )
            (UserVar "subtract"), TopLam (([(UserVar "x_boxed");(UserVar "y_boxed")],[],[(UserVar "x");(UserVar "y");(UserVar "r")],[]),
                Case(
                    App((UserVar "x_boxed"), []),
                    (UserVar "x_boxed"),
                    AAlts (
                        [(UserVar "Int",[(UserVar "x")]),
                            Case(
                                App((UserVar "y_boxed"), []),
                                (UserVar "y_boxed"),
                                AAlts (
                                    [(UserVar "Int",[(UserVar "y")]),
                                        Case(
                                            Prim [ALit Wasm.I32Sub; AVar (UserVar "x"); AVar (UserVar "y"); ],
                                            (UserVar "r"),
                                            PAlts([], 
                                                Constr (UserVar "Int", [AVar (UserVar "r")])))                                    
                                    ],
                                    Prim [ALit Wasm.Unreachable; ALit (I32Const 0)]
                                )
                            )
                        ],
                        Prim [ALit Wasm.Unreachable; ALit (I32Const 0)]
                    )
                )            
            )
            (UserVar "Fibonacci"),  TopLam (([(UserVar "x")],[],[(UserVar "x_boxed"); (UserVar "result"); (UserVar "return")],[]),
                Case(
                    Constr(UserVar "Int", [AVar (UserVar "x")]), 
                    (UserVar "x_boxed"),
                    PAlts ([],
                        Case(
                            Prim[AVar (UserVar "fibonacci_boxed");  AVar((UserVar "x_boxed"))],
                            (UserVar "result"),
                            AAlts(
                                [(UserVar "Int", [(UserVar "return")]),
                                    Prim [AVar (UserVar "return")]
                                ],
                                Prim [ALit Wasm.Unreachable; ALit (I32Const 0)]
                            )
                        )
                    )
                )               
            )
            (UserVar "fibonacci_boxed"), TopLam (([(UserVar "x_boxed")],[],[(UserVar "one"); (UserVar "two"); (UserVar "x_minus_one");(UserVar "x_minus_two");(UserVar "a");(UserVar "b"); (UserVar "x")],[]),
                Case(
                    App((UserVar "x_boxed"),[]),
                    (UserVar "x_boxed"),
                    AAlts(
                        [(UserVar "Int", [(UserVar "x")]),
                            Case(
                                Prim[AVar (UserVar "x")], 
                                (UserVar "x"),
                                PAlts (
                                    [
                                        Wasm.I32Const 0, Constr (UserVar "Int", [ALit (Wasm.I32Const 1)])
                                        Wasm.I32Const 1, Constr (UserVar "Int", [ALit (Wasm.I32Const 1)])
                                    ],
                                    Case(
                                        Constr (UserVar "Int", [ALit (Wasm.I32Const 1)]),
                                        (UserVar "one"),
                                        PAlts(
                                            [],
                                            Case(
                                                Constr (UserVar "Int", [ALit (Wasm.I32Const 2)]),
                                                (UserVar "two"),
                                                PAlts(
                                                    [],
                                                    Case(
                                                        Prim[AVar(UserVar "subtract"); AVar (UserVar "x_boxed");  AVar (UserVar "one")],
                                                        (UserVar "x_minus_one"),
                                                        PAlts(
                                                            [],
                                                            Case(
                                                                Prim[AVar(UserVar "subtract"); AVar (UserVar "x_boxed");  AVar (UserVar "two")],
                                                                (UserVar "x_minus_two"),
                                                                PAlts(
                                                                    [],                                            
                                                                    Case(
                                                                        Prim[AVar (UserVar "fibonacci_boxed"); AVar (UserVar "x_minus_one")],
                                                                        (UserVar "a"),
                                                                        PAlts(
                                                                            [],
                                                                            Case(
                                                                                Prim[AVar (UserVar "fibonacci_boxed");  AVar (UserVar "x_minus_two")],
                                                                                (UserVar "b"),
                                                                                PAlts([], 
                                                                                    Prim[AVar(UserVar "add"); AVar (UserVar "a"); AVar (UserVar "b")])
                                                                           )        
                                                                        )     
                                                                    )                                                      
                                                                )
                                                            )  
                                                        )         
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        ],
                        Prim [ALit Wasm.Unreachable; ALit (I32Const 0)]             
                    )
                )
            )        
        ]
    let fibonacciProgram =
        stgModule
        |> WasmGen.genProgram
        |> emitWasmModule
        |> compile

    let output = fibonacciProgram.Exports?Fibonacci (7)
    Assert.Equal(21, output)
