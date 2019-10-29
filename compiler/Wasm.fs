module Wasm

type Name = string

type Vec<'t> = list<'t>

type ValType =
    | I32
    | I64
    | F32
    | F64

type BlockType = ValType list

type FuncType = Vec<ValType> * Vec<ValType>

type Limits =
    | Min of uint32
    | MinMax of uint32 * uint32

type MemType = Limits

type ElemType = FuncRef

type TableType = ElemType * Limits

type Mut =
    | Const
    | Var

type GlobalType = ValType * Mut

type TypeIdx = uint32

type FuncIdx = uint32

type TableIdx = uint32

type MemIdx = uint32

type GlobalIdx = uint32

type LocalIdx = uint32

type LabelIdx = uint32

type MemArg =
    { align: uint32
      offset: uint32 }

type Instr =
    // Control
    | Unreachable
    | Nop
    | Block of BlockType * Instr list
    | Loop of BlockType * Instr list
    | If of BlockType * Instr list
    | IfElse of BlockType * Instr list * Instr List
    | Br of LabelIdx
    | BrIf of LabelIdx
    | BrTable of LabelIdx list * LabelIdx
    | Return
    | Call of FuncIdx
    | CallIndirect of TypeIdx
    // Parametric
    | Drop
    | Select
    // Variable
    | LocalGet of LocalIdx
    | LocalSet of LocalIdx
    | LocalTee of LocalIdx
    | GlobalGet of GlobalIdx
    | GlobalSet of GlobalIdx
    // Memory
    | I32Load
    | I64Load of MemArg
    | F32Load of MemArg
    | F64Load of MemArg
    | I32Load8S of MemArg
    | I32Load8U of MemArg
    | I32Load16S of MemArg
    | I32Load16U of MemArg
    | I64Load8S of MemArg
    | I64Load8U of MemArg
    | I64Load16S of MemArg
    | I64Load16U of MemArg
    | I64Load32S of MemArg
    | I64Load32U of MemArg
    | I32Store of MemArg
    | I64Store of MemArg
    | F32Store of MemArg
    | F64Store of MemArg
    | I32Store8 of MemArg
    | I32Store16 of MemArg
    | I64Store8 of MemArg
    | I64Store16 of MemArg
    | I64Store32 of MemArg
    | MemorySize
    | MemoryGrow
    // Numeric const
    | I32Const of uint32
    | I64Const of uint64
    | F32Const of float32
    | F64Const of float
    // Numeric
    | I32Eqz
    | I32Eq
    | I32Ne
    | I32LtS
    | I32GtS
    | I32LtU
    | I32GtU
    | I32LeS
    | I32GeS
    | I32LeU
    | I32GeU

    | I64Eqz
    | I64Eq
    | I64Ne
    | I64LtS
    | I64GtS
    | I64LtU
    | I64GtU
    | I64LeS
    | I64GeS
    | I64LeU
    | I64GeU

    | F32Eq
    | F32Ne
    | F32Lt
    | F32Gt
    | F32Le
    | F32Ge

    | F64Eq
    | F64Ne
    | F64Lt
    | F64Gt
    | F64Le
    | F64Ge

    | I32Clz
    | I32Ctz
    | I32PopCnt
    | I32Add
    | I32Sub
    | I32Mul
    | I32DivS
    | I32DivU
    | I32RemS
    | I32RemU
    | I32And
    | I32Or
    | I32Xor
    | I32Shl
    | I32ShrS
    | I32ShrU
    | I32RotL
    | I32RotR

    | I64Clz
    | I64Ctz
    | I64PopCnt
    | I64Add
    | I64Sub
    | I64Mul
    | I64DivS
    | I64DivU
    | I64RemS
    | I64RemU
    | I64And
    | I64Or
    | I64Xor
    | I64Shl
    | I64ShrS
    | I64ShrU
    | I64RotL
    | I64RotR

    | F32Abs
    | F32Neg
    | F32Ceil
    | F32Floor
    | F32Trunc
    | F32Nearest
    | F32Sqrt
    | F32Add
    | F32Sub
    | F32Mul
    | F32Div
    | F32Min
    | F32Max
    | F32CopySign

    | F64Abs
    | F64Neg
    | F64Ceil
    | F64Floor
    | F64Trunc
    | F64Nearest
    | F64Sqrt
    | F64Add
    | F64Sub
    | F64Mul
    | F64Div
    | F64Min
    | F64Max
    | F64CopySign

    | I32WrapI64
    | I32TrucF32S
    | I32TruncF32U
    | I32TrncF64S
    | I32TructF64U
    | I64ExtendI32S
    | I64ExtendI32U
    | I64TruncF32S
    | I64TruncF32U
    | I64TruncF64S
    | I64TruncF64U
    | F32ConvertI32S
    | F32ConvertI32U
    | F32ConvertI64S
    | F32ConvertI64U
    | F32DemoteF64
    | F64ConvertI32S
    | F64ConvertI32U
    | F64ConvertI64S
    | F64ConvertI64U
    | F64PromoteF32
    | I32ReinterpretF32
    | I64ReinterpretF64
    | F32ReinterpretI32
    | F64ReinterpretI64

type Expr = Instr list

type Custom = Name * Vec<byte>

type CustomSec = Custom

type TypeSec = Vec<FuncType>

type ImportDesc =
    | ImportFunc of TypeIdx
    | ImportTable of TableType
    | ImportMem of MemType
    | ImportGlobal of GlobalType

type Import =
    { modulename: Name
      nm: Name
      desc: ImportDesc }

type ImportSec = Vec<Import>

type FuncSec = Vec<TypeIdx>

type Table = TableType

type TableSec = Vec<Table>

type Mem = MemType

type MemSec = Vec<Mem>

type Global =
    { gt: GlobalType
      init: Expr }

type GlobalSec = Vec<Global>

type ExportDesc =
    | ExportFunc of FuncIdx
    | ExportTable of TableIdx
    | ExportMem of MemIdx
    | ExportGlobal of GlobalIdx

type Export =
    { nm: Name
      desc: ExportDesc }

type ExportSec = Vec<Export>

type Start =
    { func: FuncIdx }

type StartSec = Start

type Elem =
    { table: TableIdx
      offset: Expr
      Init: Vec<FuncIdx> }

type ElemSec = Vec<Elem>

type Locals = uint32 * ValType

type Func = Vec<Locals> * Expr

type Code = Func

type CodeSec = Vec<Code>

type Data =
    { data: MemIdx
      offset: Expr
      init: byte array }

type DataSec = Vec<Data>

type Section =
    | CustomSec of CustomSec
    | TypeSec of TypeSec
    | ImportSec of ImportSec
    | FuncSec of FuncSec
    | TableSec of TableSec
    | MemSec of MemSec
    | GlobalSec of GlobalSec
    | ExportSec of ExportSec
    | StartSec of StartSec
    | ElemSec of ElemSec
    | CodeSec of CodeSec
    | DataSec of DataSec

type WasmModule = Section list
