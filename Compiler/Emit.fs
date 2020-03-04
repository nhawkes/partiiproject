module Emit

open Wasm

let rec emitInt64U (x:uint64) = 
    [
        let b = (x &&& 0x7FuL) |>byte
        let v = x>>>7
        if v = 0uL then
            b
        else
            b|||0x80uy
            yield! emitInt64U v    
    ]

let rec emitInt64S (x:int64) = 
    [        
        let b = (x&&&0x7FL)|>byte
        let v = x>>>7
        if v = 0L && ((b &&& 0x40uy) = 0uy) || v = -1L  && ((b &&& 0x40uy) <> 0uy) then
            b
        else
            b|||0x80uy
            yield! emitInt64S v    
    ]

let emitInt32U: uint32 -> byte list = uint64 >> emitInt64U
let emitInt32S: int32 -> byte list = int64 >> emitInt64S
let emitFloat32 (x:float32) = x |> System.BitConverter.GetBytes |> Array.toList
let emitFloat64 (x:float) = x |> System.BitConverter.GetBytes |> Array.toList

let emitVec f x =
    [
        x |> List.length |> uint32 |> emitInt32U
        x |> List.collect f
    ] |> List.concat
    

let emitName (x:string) = 
    let utf8 = System.Text.UTF8Encoding()
    let length = x |> String.length |> uint32
    let bytes = utf8.GetBytes x
    [
        length |> emitInt32U
        bytes |> Array.toList
    ] |> List.concat

let emitValType = 
    function
    | I32 -> 0x7Fuy
    | I64 -> 0x7Euy
    | F32 -> 0x7Duy
    | F64 -> 0x7Cuy

let emitBlockType = function
    |[] -> 0x40uy
    |[t] -> emitValType t
    |_ -> failwith "illegal"

let emitFuncType (a, b) = 
    [
        [0x60uy]
        emitVec (emitValType >> List.singleton) a
        emitVec (emitValType >> List.singleton) b
    ] |> List.concat

let emitLimits = function
    | Min min -> [0x00uy; yield! emitInt32U min]
    | MinMax (min, max) -> [0x01uy; yield! emitInt32U min; yield! emitInt32U max]

let emitMemType = emitLimits

let emitElemType = function
    |FuncRef -> [0x70uy]

let emitTableType (elemType, limits) =
    [
        emitElemType elemType
        emitLimits limits        
    ] |> List.concat

let emitMut = function
    | Const -> 0x00uy
    | Var -> 0x01uy

let emitGlobalType (t, m) = 
    [
        emitValType t
        emitMut m
    ]

let emitTypeIdx: TypeIdx -> byte list = emitInt32U
let emitFuncIdx: FuncIdx -> byte list = emitInt32U
let emitTableIdx: TableIdx -> byte list = emitInt32U
let emitMemIdx: MemIdx -> byte list = emitInt32U
let emitGlobalIdx: GlobalIdx -> byte list = emitInt32U
let emitLocalIdx: LocalIdx -> byte list = emitInt32U
let emitLabelIdx: LabelIdx -> byte list = emitInt32U

let emitMemArg { align=a; offset=o } =
    [
        emitInt32U a
        emitInt32U o
    ] |> List.concat

let rec emitInstr instr =
    let emitInstrs instr = List.collect (emitInstr) instr
    match instr with
    // Control
    | Unreachable -> [0x00uy]
    | Nop -> [0x01uy]
    | Block (rt, instrs) ->
        [0x02uy; emitBlockType rt; yield! emitInstrs instrs; 0x0Buy]
    | Loop (rt, instrs) ->
        [0x03uy; emitBlockType rt; yield! emitInstrs instrs; 0x0Buy]
    | If (rt, instrs) ->
        [0x04uy; emitBlockType rt; yield! emitInstrs instrs; 0x0Buy]
    | IfElse (rt, instrs1, instrs2) ->
        [0x04uy; emitBlockType rt; yield! emitInstrs instrs1; 0x05uy; yield! emitInstrs instrs2; 0x0Buy]
    | Br (l) -> [0x0Cuy; yield! emitLabelIdx l]
    | BrIf (l) -> [0x0Duy; yield! emitLabelIdx l]
    | BrTable (ls, ln) -> [0x0Cuy; yield! emitVec emitLabelIdx ls; yield! emitLabelIdx ln]
    | Return -> [0x0Fuy]
    | Call (x) -> [0x10uy; yield! emitFuncIdx x]
    | CallIndirect (x) -> [0x11uy; yield! emitTypeIdx x; 0x00uy]
    // Parametric
    | Drop -> [0x1Auy]
    | Select -> [0x1Buy]
    // Variable
    | LocalGet x -> [0x20uy; yield! emitLocalIdx x]
    | LocalSet x -> [0x21uy; yield! emitLocalIdx x]
    | LocalTee x -> [0x22uy; yield! emitLocalIdx x]
    | GlobalGet x -> [0x23uy; yield! emitGlobalIdx x]
    | GlobalSet x -> [0x24uy; yield! emitGlobalIdx x]
    // Memory
    | I32Load m -> [0x28uy; yield! emitMemArg m]
    | I64Load m -> [0x29uy; yield! emitMemArg m]
    | F32Load m -> [0x2Auy; yield! emitMemArg m]
    | F64Load m -> [0x2Buy; yield! emitMemArg m]
    | I32Load8S m -> [0x2Cuy; yield! emitMemArg m]
    | I32Load8U m -> [0x2Duy; yield! emitMemArg m]
    | I32Load16S m -> [0x2Euy; yield! emitMemArg m]
    | I32Load16U m -> [0x2Fuy; yield! emitMemArg m]
    | I64Load8S m -> [0x30uy; yield! emitMemArg m]
    | I64Load8U m -> [0x31uy; yield! emitMemArg m]
    | I64Load16S m -> [0x32uy; yield! emitMemArg m]
    | I64Load16U m -> [0x33uy; yield! emitMemArg m]
    | I64Load32S m -> [0x34uy; yield! emitMemArg m]
    | I64Load32U m -> [0x35uy; yield! emitMemArg m]
    | I32Store m -> [0x36uy; yield! emitMemArg m]
    | I64Store m -> [0x37uy; yield! emitMemArg m]
    | F32Store m -> [0x38uy; yield! emitMemArg m]
    | F64Store m -> [0x39uy; yield! emitMemArg m]
    | I32Store8 m -> [0x3Auy; yield! emitMemArg m]
    | I32Store16 m -> [0x3Buy; yield! emitMemArg m]
    | I64Store8 m -> [0x3Cuy; yield! emitMemArg m]
    | I64Store16 m -> [0x3Duy; yield! emitMemArg m]
    | I64Store32 m -> [0x3Euy; yield! emitMemArg m]
    | MemorySize -> [0x3Fuy; 0x00uy]
    | MemoryGrow -> [0x40uy; 0x00uy]
    // Numeric const
    | I32Const x -> [0x41uy; yield! emitInt32S x]
    | I64Const x -> [0x42uy; yield! emitInt64S x]
    | F32Const x -> [0x43uy; yield! emitFloat32 x]
    | F64Const x -> [0x44uy; yield! emitFloat64 x]
    // Numeric    
    | I32Eqz -> [0x45uy]
    | I32Eq -> [0x46uy]
    | I32Ne -> [0x47uy]
    | I32LtS -> [0x48uy]
    | I32LtU -> [0x49uy]
    | I32GtS -> [0x4Auy]
    | I32GtU -> [0x4Buy]
    | I32LeS -> [0x4Cuy]
    | I32LeU -> [0x4Duy]
    | I32GeS -> [0x4Euy]
    | I32GeU -> [0x4Fuy]

    | I64Eqz -> [0x50uy]
    | I64Eq -> [0x51uy]
    | I64Ne -> [0x52uy]
    | I64LtS -> [0x53uy]
    | I64LtU -> [0x54uy]
    | I64GtS -> [0x55uy]
    | I64GtU -> [0x56uy]
    | I64LeS -> [0x57uy]
    | I64LeU -> [0x58uy]
    | I64GeS -> [0x59uy]
    | I64GeU -> [0x5Auy]

    | F32Eq -> [0x5Buy]
    | F32Ne -> [0x5Cuy]
    | F32Lt -> [0x5Duy]
    | F32Gt -> [0x5Euy]
    | F32Le -> [0x5Fuy]
    | F32Ge -> [0x60uy]

    | F64Eq -> [0x61uy]
    | F64Ne -> [0x62uy]
    | F64Lt -> [0x63uy]
    | F64Gt -> [0x64uy]
    | F64Le -> [0x65uy]
    | F64Ge -> [0x66uy]

    | I32Clz -> [0x67uy]
    | I32Ctz -> [0x68uy]
    | I32PopCnt -> [0x69uy]
    | I32Add -> [0x6Auy]
    | I32Sub -> [0x6Buy]
    | I32Mul -> [0x6Cuy]
    | I32DivS -> [0x6Duy]
    | I32DivU -> [0x6Euy]
    | I32RemS -> [0x6Fuy]
    | I32RemU -> [0x70uy]
    | I32And -> [0x71uy]
    | I32Or -> [0x72uy]
    | I32Xor -> [0x73uy]
    | I32Shl -> [0x74uy]
    | I32ShrS -> [0x75uy]
    | I32ShrU -> [0x76uy]
    | I32RotL -> [0x77uy]
    | I32RotR -> [0x78uy]

    | I64Clz -> [0x79uy]
    | I64Ctz -> [0x7Auy]
    | I64PopCnt -> [0x7Buy]
    | I64Add -> [0x7Cuy]
    | I64Sub -> [0x7Duy]
    | I64Mul -> [0x7Euy]
    | I64DivS -> [0x7Fuy]
    | I64DivU -> [0x80uy]
    | I64RemS -> [0x81uy]
    | I64RemU -> [0x82uy]
    | I64And -> [0x83uy]
    | I64Or -> [0x84uy]
    | I64Xor -> [0x85uy]
    | I64Shl -> [0x86uy]
    | I64ShrS -> [0x87uy]
    | I64ShrU -> [0x88uy]
    | I64RotL -> [0x89uy]
    | I64RotR -> [0x8Auy]

    | F32Abs -> [0x8Buy]
    | F32Neg -> [0x8Cuy]
    | F32Ceil -> [0x8Duy]
    | F32Floor -> [0x8Euy]
    | F32Trunc -> [0x8Fuy]
    | F32Nearest -> [0x90uy]
    | F32Sqrt -> [0x91uy]
    | F32Add -> [0x92uy]
    | F32Sub -> [0x93uy]
    | F32Mul -> [0x94uy]
    | F32Div -> [0x95uy]
    | F32Min -> [0x96uy]
    | F32Max -> [0x97uy]
    | F32CopySign -> [0x98uy]

    | F64Abs -> [0x99uy]
    | F64Neg -> [0x9Auy]
    | F64Ceil -> [0x9Buy]
    | F64Floor -> [0x9Cuy]
    | F64Trunc -> [0x9Duy]
    | F64Nearest -> [0x9Euy]
    | F64Sqrt -> [0x9Fuy]
    | F64Add -> [0xA0uy]
    | F64Sub -> [0xA1uy]
    | F64Mul -> [0xA2uy]
    | F64Div -> [0xA3uy]
    | F64Min -> [0xA4uy]
    | F64Max -> [0xA5uy]
    | F64CopySign -> [0xA6uy]

    | I32WrapI64 -> [0xA7uy]
    | I32TrucF32S -> [0xA8uy]
    | I32TruncF32U -> [0xA9uy]
    | I32TruncF64S -> [0xAAuy]
    | I32TructF64U -> [0xABuy]
    | I64ExtendI32S -> [0xACuy]
    | I64ExtendI32U -> [0xADuy]
    | I64TruncF32S -> [0xAEuy]
    | I64TruncF32U -> [0xAFuy]
    | I64TruncF64S -> [0xB0uy]
    | I64TruncF64U -> [0xB1uy]
    | F32ConvertI32S -> [0xB2uy]
    | F32ConvertI32U -> [0xB3uy]
    | F32ConvertI64S -> [0xB4uy]
    | F32ConvertI64U -> [0xB5uy]
    | F32DemoteF64 -> [0xB6uy]
    | F64ConvertI32S -> [0xB7uy]
    | F64ConvertI32U -> [0xB8uy]
    | F64ConvertI64S -> [0xB9uy]
    | F64ConvertI64U -> [0xBAuy]
    | F64PromoteF32 -> [0xBBuy]
    | I32ReinterpretF32 -> [0xBCuy]
    | I64ReinterpretF64 -> [0xBDuy]
    | F32ReinterpretI32 -> [0xBEuy]
    | F64ReinterpretI64 -> [0xBFuy]

let emitExpr instrs =
    [
        instrs |> List.collect emitInstr
        [0x0Buy]
    ] |> List.concat

let emitSec n f x =
    let b = f x
    [
        [n]
        b |> List.length |> uint32 |> emitInt32U
        b
    ] |> List.concat

let emitNameSubSection i f x =
    let bytes = f x
    let size = bytes |> List.length |> uint32 |> emitInt32U
    [
        [i]
        size
        bytes
    ] |> List.concat

let emitNameAssoc (idx, name) = [emitInt32U idx; emitName name] |> List.concat
let emitNameMap = emitVec emitNameAssoc
let emitIndirectNameAssoc(idx, namemap) = [emitInt32U idx; emitNameMap namemap] |> List.concat
let emitIndirectNameMap = emitVec emitIndirectNameAssoc
let emitModuleNameSubsec = emitNameSubSection 0x00uy emitName
let emitFuncNameSubsec = emitNameSubSection 0x01uy emitNameMap
let emitLocalNameSubsec = emitNameSubSection 0x02uy emitIndirectNameMap

let emitCustom = function
    |Custom(name, bytes) ->
        [
            emitName name
            emitVec List.singleton bytes 
        ] |> List.concat
    |NameSec{moduleNameSubsec=moduleNameSubsec;funcNameSubsec=funcNameSubsec;localNameSubsec=localNameSubsec} ->
        [
            emitName "name"
            emitModuleNameSubsec moduleNameSubsec
            emitFuncNameSubsec funcNameSubsec
            emitLocalNameSubsec localNameSubsec

        ] |> List.concat

let emitCustomSec = 
    emitCustom |> emitSec 0x00uy

let emitTypeSec = 
    emitVec emitFuncType |> emitSec 0x01uy

let emitImportDesc = function
    | ImportFunc x -> [0x00uy; yield! emitTypeIdx x]
    | ImportTable tt -> [0x01uy; yield! emitTableType tt]
    | ImportMem mt -> [0x02uy; yield! emitMemType mt]
    | ImportGlobal gt -> [0x03uy; yield! emitGlobalType gt]

let emitImport { modulename=modulename; nm=nm; importdesc=desc } =
      [
          emitName modulename
          emitName nm
          emitImportDesc desc
      ] |> List.concat

let emitImportSec = 
    emitVec emitImport |> emitSec 0x02uy

let emitFuncSec = 
    emitVec emitTypeIdx |> emitSec 0x03uy

let emitTable = emitTableType

let emitTableSec =
    emitVec emitTable |> emitSec 0x04uy

let emitMem = emitMemType

let emitMemSec = 
    emitVec emitMem |> emitSec 0x05uy

let emitGlobal { gt=gt; init=init } =
    [
        emitGlobalType gt
        emitExpr init
    ] |> List.concat

let emitGlobalSec = 
    emitVec emitGlobal |> emitSec 0x06uy

let emitExportDesc = function
    | ExportFunc x -> [0x00uy; yield! emitFuncIdx x]
    | ExportTable x -> [0x01uy; yield! emitTableIdx x]
    | ExportMem x -> [0x02uy; yield! emitMemIdx x]
    | ExportGlobal x -> [0x03uy; yield! emitGlobalIdx x]

let emitExport { nm=nm; exportdesc=desc } =
    [
        emitName nm
        emitExportDesc desc
    ] |> List.concat

let emitExportSec = 
    emitVec emitExport |> emitSec 0x07uy
    

let emitStart { func=func } =
    emitFuncIdx func

let emitStartSec = 
    emitStart |> emitSec 0x08uy

let emitElem { table=table; offset=offset; init=init} =
    [
        emitTableIdx table
        emitExpr offset
        emitVec emitFuncIdx init
    ] |> List.concat
    
let emitElemSec =
    emitVec emitElem |> emitSec 0x09uy

let emitLocal (n, t) =
    [ 
        emitInt32U n
        [emitValType t]
    ] |> List.concat

let rec emitLocals acc = function
    |[] -> 
        [
            acc |> List.length |> uint32 |> emitInt32U
            acc |> List.rev |> List.collect emitLocal
        ] |> List.concat
    |t::ts ->
        let n = ts |> List.takeWhile ((=) t) |> List.length |> (+) 1 
        t::ts |> List.skip n |> emitLocals ((n |> uint32, t)::acc)


let emitFunc (locals, expr) =
    [
        emitLocals [] locals
        emitExpr expr
    ] |> List.concat


let emitCode (code:Code) =
    let bytes = emitFunc code
    let size = bytes |> List.length |> uint32 |> emitInt32U
    [
        size
        bytes
    ] |> List.concat

let emitCodeSec = 
    emitVec emitCode |> emitSec 0x0Auy

let emitData { data=x; offset=e; init=bytes} =
    [
        emitMemIdx x
        emitExpr e
        Array.length bytes |> uint32 |> emitInt32U
        Array.toList bytes
    ] |> List.concat


let emitDataSec = 
    emitVec emitData |> emitSec 0x0Buy

let emitSection = function
    | CustomSec x -> emitCustomSec x
    | TypeSec x -> emitTypeSec x
    | ImportSec x -> emitImportSec x
    | FuncSec x -> emitFuncSec x
    | TableSec x -> emitTableSec x
    | MemSec x -> emitMemSec x
    | GlobalSec x -> emitGlobalSec x
    | ExportSec x -> emitExportSec x
    | StartSec x -> emitStartSec x
    | ElemSec x -> emitElemSec x
    | CodeSec x -> emitCodeSec x
    | DataSec x -> emitDataSec x

let emitWasmModule sections =
    let magic = [0x00uy; 0x61uy; 0x73uy; 0x6Duy]
    let version = [0x01uy; 0x00uy; 0x00uy; 0x00uy]
    [
        magic
        version        
        sections |> List.collect emitSection
    ] |> List.concat
