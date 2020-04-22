module IntegrationTests

open Xunit
open Emit
open Core
open Utils



[<Fact>]
let ``Fibinacci 7``() =
    let program = """
        export Fibonacci(x) = {
            return switch(x){
                | 0 => 1
                | 1 => 1
                | _ => Fibonacci(x-1) + Fibonacci(x-2)
            }
        }
        """
    match Parser.parseString program with
    | Error err -> failwith err
    | Ok astModule ->
        let coreModule = astModule |> CoreGen.genProgram

        let fv = coreModule |> fvProgram
        match fv |> Set.toList with
        | (x, _) :: _ -> failwithf "Not defined %s" x
        | _ ->

            let coreModule = coreModule |> Transform.transform true
            let stgModule = coreModule |> StgGen.genProgram
            let moduleName = "test"
            let wasmModule = stgModule |> WasmGen.genProgram moduleName
            let fibonacciProgram = emitWasmModule wasmModule |> compile
            let output = fibonacciProgram.Exports?Fibonacci (7)
            Assert.Equal(21, output)




[<Fact>]
let ``Optimised Fibinacci 7``() =
    let program = """
        export Fibonacci(x) = {
            return switch(x){
                | 0 => 1
                | 1 => 1
                | _ => Fibonacci(x-1) + Fibonacci(x-2)
            }
        }
        """
    match Parser.parseString program with
    | Error err -> failwith err
    | Ok astModule ->
        let coreModule = astModule |> CoreGen.genProgram

        let fv = coreModule |> fvProgram
        match fv |> Set.toList with
        | (x, _) :: _ -> failwithf "Not defined %s" x
        | _ ->

            let coreModule = coreModule |> Transform.transform false
            let stgModule = coreModule |> StgGen.genProgram
            let moduleName = "test"
            let wasmModule = stgModule |> WasmGen.genProgram moduleName

            let fibonacciProgram = emitWasmModule wasmModule |> compile
            let output = fibonacciProgram.Exports?Fibonacci (7)
            Assert.Equal(21, output)

[<Fact>]
let ``Partial application``() =
    let program = """
        export Three() = {
            f(a,b)=a+b
            addOne = f(1)
            return addOne(2)
        }
        """
    match Parser.parseString program with
    | Error err -> failwith err
    | Ok astModule ->
        let coreModule = astModule |> CoreGen.genProgram

        let fv = coreModule |> fvProgram
        match fv |> Set.toList with
        | (x, _) :: _ -> failwithf "Not defined %s" x
        | _ ->

            let coreModule = coreModule |> Transform.transform false
            let stgModule = coreModule |> StgGen.genProgram
            let moduleName = "test"
            let wasmModule = stgModule |> WasmGen.genProgram moduleName

            let fibonacciProgram = emitWasmModule wasmModule |> compile
            let output = fibonacciProgram.Exports?Three ()
            Assert.Equal(3, output)


[<Fact>]
let DataType() =
    let program = """
        data Wrap(i)
        export Three() = {
            return switch(Wrap(3)){
                |Wrap(i)=>i
            }
        }
        """
    match Parser.parseString program with
    | Error err -> failwith err
    | Ok astModule ->
        let coreModule = astModule |> CoreGen.genProgram

        let fv = coreModule |> fvProgram
        match fv |> Set.toList with
        | (x, _) :: _ -> failwithf "Not defined %s" x
        | _ ->

            let coreModule = coreModule |> Transform.transform false
            let stgModule = coreModule |> StgGen.genProgram
            let moduleName = "test"
            let wasmModule = stgModule |> WasmGen.genProgram moduleName

            let fibonacciProgram = emitWasmModule wasmModule |> compile
            let output = fibonacciProgram.Exports?Three ()
            Assert.Equal(3, output)
