module Types

type FuncKind =
    |SatFunc
    |UnSatFunc

type Typ =
    |FuncT of Typ * Typ
    |IntT    
    |ValueT

let rec applyArgs f args = 
    match f, args with
    |t, [] -> t
    |FuncT(a1, b), a2::xs when a1=a2 -> applyArgs b xs
    |_ -> failwith "Incorrect types"
    
let rec createFuncT funcKind args ret =
    match args with
    |a::args -> FuncT(a, createFuncT funcKind args ret)
    |[] -> ret   
