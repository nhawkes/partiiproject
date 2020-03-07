module Parser

open FParsec
open FParsec.CharParsers
open FParsec.Primitives
open Ast
open Types

let ws = skipMany (skipChar ' ') <?> "whitespace"
let ps s = skipString s .>> spaces

let pexpr, pexprImpl = createParserForwardedToRef()
let pblock, pblockImpl = createParserForwardedToRef()

let isIdentifierStartChar c = isAsciiLetter c || isAnyOf [ '_' ] c

let isTIdentifierStartChar c = isAsciiUpper c

let isIdentifierContinueChar c = isAsciiLetter c || isAnyOf [ '_' ] c || isDigit c

let pidentifier =
    pipe3
        <| getPosition 
        <| identifier (IdentifierOptions(isAsciiIdStart = isIdentifierStartChar, isAsciiIdContinue = isIdentifierContinueChar))
        <| getPosition
        <| fun startPos text endPos -> {startPos=startPos; text=text; endPos=endPos}
    .>> ws <?> "Identifier"
    

let ptidentifier =
    pipe3
        <| getPosition 
        <| identifier (IdentifierOptions(isAsciiIdStart = isTIdentifierStartChar, isAsciiIdContinue = isIdentifierContinueChar))
        <| getPosition
        <| fun startPos text endPos -> {startPos=startPos; text=text; endPos=endPos}
    .>> ws <?> "Data Identifier"

let pinteger =
    pint32 .>> ws |>> (string
                       >> int
                       >> Integer
                       >> Box)

let pliteralvalue = choice [ pinteger ]
let pliteral = pliteralvalue |>> Lit

let pbracketed = between (ps "(") (ps ")") pexpr
let pbraced = between (ps "{") (ps "}") pblock

let pargs = between (ps "(") (ps ")") (sepBy pexpr (ps ","))

let pcall =
    pidentifier .>>. opt pargs |>> function
    | (v, None) -> Var(v)
    | (v, Some args) -> Call(v, args)

let ppat, ppatImpl = createParserForwardedToRef()
let ppatlit = pliteralvalue |>> PatLit
let ppatconstr = ptidentifier .>>. between (ps "(") (ps ")") (sepBy ppat (ps ",")) |>> PatConstr
let ppatbind = pidentifier |>> PatBind

ppatImpl := choice [ ppatconstr; ppatlit; ppatbind ]

let pcaseexpr = choice [ spaces >>. pexpr <??> "Switch expression" ]
let pcase = ps "|" >>. ppat .>> ps "=>" .>>. pcaseexpr |>> Case
let pswitchblock = between (ps "{") (ps "}") (many (pcase .>> spaces))
let pswitch =
    ps "switch" >>. between (ps "(") (ps ")") pexpr .>>. pswitchblock |>> function
    | (e, block) -> Match((e, ValueT), block)


let pif = ps "if" >>. pexpr .>>. (ps "then" >>. pexpr .>> spaces .>> ps "else" .>>. pexpr) |>> If


let pterm = choice [ pif; pswitch; pbracketed; pbraced; pliteral; pcall ]

let pbinop o x y = BinOp(x, o, y)
let operatorPrecedenceParser = OperatorPrecedenceParser<Expr, unit, unit>()

operatorPrecedenceParser.AddOperator(InfixOperator("+", ws, 5, Associativity.Left, pbinop Add))
operatorPrecedenceParser.AddOperator(InfixOperator("-", ws, 5, Associativity.Left, pbinop Sub))
operatorPrecedenceParser.AddOperator(InfixOperator("*", ws, 5, Associativity.Left, pbinop Mul))
operatorPrecedenceParser.AddOperator(InfixOperator("/", ws, 5, Associativity.Left, pbinop Div))
operatorPrecedenceParser.AddOperator(InfixOperator("=", ws, 5, Associativity.Left, pbinop Equals))
operatorPrecedenceParser.AddOperator(InfixOperator("<", ws, 5, Associativity.Left, pbinop LessThan))
operatorPrecedenceParser.TermParser <- pterm

pexprImpl := operatorPrecedenceParser.ExpressionParser


let passignVarArgs = between (ps "(") (ps ")") (sepBy pidentifier (ps ","))
let passignLHSVars = pidentifier .>>. (opt passignVarArgs .>> ws)
let passignLHS, passignLHSImpl = createParserForwardedToRef()
let passignArgs = between (ps "(") (ps ")") (sepBy passignLHS (ps ","))

passignLHSImpl := pidentifier .>>. (opt passignArgs .>> ws) |>> function
                  | v, None -> AssignVar v
                  | f, Some xs -> AssignFunc(f, xs)

let passign = passignLHS .>>. ((ps "=") >>. pexpr)
let passignStatement =
    passign <??> "Assignment" |>> function
    | (lhs, e) -> Assign(lhs, e)

let preturn = ps "return" >>. pexpr

let preturnStatement = preturn <??> "Return" |>> Return
let pstatement = choice [ preturnStatement; passignStatement ]

pblockImpl := many (pstatement .>> spaces) |>> Block

let pexportDecl =
    ps "export" >>. passignLHSVars .>>. ((ps "=") >>. pexpr) |>> function
    | ((v, None), e) -> ExportDecl((v.text, []), (v, []), e)
    | ((f, Some args), e) -> ExportDecl((f.text, args |> List.map(fun arg -> arg.text)), (f, args), e)

let pglobalDecl =
    passignLHS .>>. ((ps "=") >>. pexpr) |>> function
    | (lhs, e) -> GlobalDecl(lhs, e)


let pdata = ps "data" >>. passignLHS .>> ws |>> TypeDecl

let pdecl = choice [ pdata; pexportDecl; pglobalDecl ]

let pprogram = spaces >>. many (pdecl .>> spaces) .>> eof

let parse s =
    match runParserOnFile pprogram () s System.Text.Encoding.Default with
    | Success(result, _, _) -> Result.Ok result
    | Failure(s, _, _) -> Result.Error s

let parseString s =
    match runParserOnString pprogram () "Program" s with
    | Success(result, _, _) -> Result.Ok result
    | Failure(s, _, _) -> Result.Error s