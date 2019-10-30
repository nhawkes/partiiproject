// Learn more about F# at http://fsharp.org

open System
open Wasm
open Emit

let wasmModule = [
    TypeSec [
       [I32], [I32] 
    ]
    FuncSec [
        0u
    ]
    ExportSec [
        {nm="fibonacci"; exportdesc=ExportFunc 0u}
    ]
    CodeSec [
        [],
        [
            LocalGet 0u
            I32Const 0
            I32Eq
            IfElse ([I32],
                [I32Const 1],                    
                [
                    LocalGet 0u
                    I32Const 1
                    I32Eq
                    IfElse ([I32],
                        [I32Const 1],
                        [
                            LocalGet 0u
                            I32Const 1
                            I32Sub
                            Call 0u

                            LocalGet 0u
                            I32Const 2
                            I32Sub
                            Call 0u

                            I32Add
                        ]                            
                    )
                ]
            )
        ]
    ]

]

[<EntryPoint>]
let main argv =
    let bytes = emitWasmModule wasmModule |> List.toArray
    IO.File.WriteAllBytes("./compiler/benchmark/out/wasm/fibonacci.wasm", bytes)
    0
