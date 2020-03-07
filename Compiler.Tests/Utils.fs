module Utils
open System

let arrayFromTuple =
    function
    | null -> [||]
    | t when Reflection.FSharpType.IsTuple(t.GetType()) -> Reflection.FSharpValue.GetTupleFields t
    | x -> [| x |]

let (?) (self: 'Source) (prop: string): obj -> 'Result =
    let p = self.GetType().GetMethod(prop)
    if (isNull p)
    then fun _ -> self.GetType().GetProperty(prop).GetValue(self) :?> 'Result
    else fun x -> p.Invoke(self, arrayFromTuple x) :?> 'Result

let compile byteList =
    let bytes = byteList |> List.toArray
    use stream = new IO.MemoryStream(bytes)
    WebAssembly.Runtime.Compile.FromBinary(stream).Invoke(Collections.Generic.Dictionary<_, _>())

    

let memoryToArray (memory: WebAssembly.Runtime.UnmanagedMemory) =
    let size = 10
    let (array: int array) = Array.create size 0
    System.Runtime.InteropServices.Marshal.Copy(memory.Start, array, 0, size)
    array