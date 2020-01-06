module Parser

open FParsec
open FParsec.CharParsers
open FParsec.Primitives
open Ast
open Types

let ws = skipMany(skipChar ' ')
let ps s = skipString s .>> spaces

let pexpr, pexprImpl = createParserForwardedToRef()
let pblock, pblockImpl = createParserForwardedToRef()

let isIdentifierStartChar c =
    isAsciiLetter c || isAnyOf ['_'] c

let isIdentifierContinueChar c =
    isAsciiLetter c || isAnyOf ['_'] c || isDigit c

let pidentifier =
    identifier (
        IdentifierOptions(isAsciiIdStart=isIdentifierStartChar, isAsciiIdContinue=isIdentifierContinueChar)
    ) .>> ws

let pinteger =
    pint32 .>> ws
    |>> (string >> int >> Integer >> Box)

let pliteralvalue = choice [pinteger] 
let pliteral = pliteralvalue |>> Lit

let pbracketed = between (ps "(") (ps ")") pexpr
let pbraced = between (ps "{") (ps "}") pblock

let pargs = between (ps "(") (ps ")") (sepBy pexpr (ps ","))
let pcall = 
    pidentifier .>>. opt pargs |>> function
    |(v, None) -> Var v
    |(v, Some args) -> Call(v, args)

let ppatlit = pliteralvalue |>> PatLit
let ppatbind = pidentifier |>> PatBind
let ppat = choice [ppatlit; ppatbind]

let pcaseexpr = choice [
    pexpr
    skipNewline >>. pblock
]
let pcase = 
    ps "|" >>. ppat .>> ps "=>" .>>. pcaseexpr
    |>> Case
let pswitchblock = 
    between (ps "{") (ps "}") (many (pcase .>> spaces))
let pswitch = 
    ps "switch" >>. between (ps "(") (ps ")") pexpr .>>. pswitchblock |>> function
    |(e, block) -> Match((e, ValueT), block)
    

let pterm = choice [pswitch; pbracketed; pbraced; pliteral; pcall]

let pbinop o x y =
    BinOp(x, o, y)
let operatorPrecedenceParser = OperatorPrecedenceParser<Expr<string>, unit, unit>()
operatorPrecedenceParser.AddOperator(InfixOperator("+", ws, 5, Associativity.Left, pbinop Add))
operatorPrecedenceParser.AddOperator(InfixOperator("-", ws, 5, Associativity.Left, pbinop Sub))
operatorPrecedenceParser.TermParser <- pterm

pexprImpl := operatorPrecedenceParser.ExpressionParser


let passignArgs = between (ps "(") (ps ")") (sepBy pidentifier (ps ","))
let passign = pidentifier .>>. (opt passignArgs .>> ws) .>>. ((ps "=") >>. pexpr)
let passignStatement = passign <??> "Assignment" |>> function
    |((v, None), e) -> Assign(v, [], e)
    |((f, Some args), e) -> Assign(f, args, e)

let preturn = 
    ps "return" >>. pexpr

let preturnStatement = preturn <??> "Return" |>> Return
let pstatement = choice [preturnStatement; passignStatement]    
pblockImpl := 
    many (pstatement .>> spaces) |>> Block

let pexportDecl = 
    ps "export" >>. passign  |>> function
    |((v, None), e) -> ExportDecl((v, []), (v, []), e)
    |((f, Some args), e) -> ExportDecl((f, args), (f, args), e)

let pglobalDecl = 
    pidentifier .>>. (opt passignArgs .>> ws) .>>. ((ps "=") >>. pexpr) |>> function
    |((v, None), e) -> GlobalDecl(v, [], e)
    |((f, Some args), e) -> GlobalDecl(f, args, e)

let pdecl = choice [pexportDecl; pglobalDecl]

let pprogram = spaces >>. many (pdecl .>> spaces) .>> eof

let parse s = 
    match runParserOnString pprogram () "Program" s with
    |Success (result, _, _) -> Result.Ok result
    |Failure(s, _, _) -> Result.Error s


