// Learn more about F# at http://fsharp.org

open System
open Stg

let stgModule : Program =
    [
        "fibonacci", ((["x"],[],["a";"b"]),
            Case(
                Prim[AVar "x"], 
                "x",
                PAlts (
                    [Wasm.I32Const 0, Prim [ALit (Wasm.I32Const 1)]], 
                    Case(
                        Prim[AVar "x"],
                        "x",
                        PAlts (
                            [Wasm.I32Const 1, Prim [ALit (Wasm.I32Const 1)]], 
                            Case(
                                Prim[AVar "fibonacci"; ALit Wasm.I32Sub; ALit (Wasm.I32Const 1); AVar "x"],
                                "a",
                                PAlts(
                                    [],
                                    Case(
                                        Prim[AVar "fibonacci"; ALit (Wasm.I32Sub); ALit (Wasm.I32Const 2); AVar("x")],
                                        "b",
                                        PAlts([], 
                                            Prim [ALit Wasm.I32Add; AVar "a"; AVar "b"]))
                                    )                
                            )
                        )
                    )
                )
            )
        )
    ]


[<EntryPoint>]
let main argv =
    let wasmModule = stgModule |> WasmGen.genProgram
    let bytes = Emit.emitWasmModule wasmModule |> List.toArray
    IO.File.WriteAllBytes("./Compiler.Benchmark/out/wasm/fibonacci.wasm", bytes)
    0
