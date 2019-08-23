module Parsers
open System
open FParsec
open ErrorExtensions
open BaseTypes

open ParserConfigs

module Signs =
    let eq = "="
    let colon = ":"
    let lightArrow = "->"
    let oPara = "("
    let cPara = ")"

    let availableOpChars = ['+'; '='; '$'; '|'; '>'; '<'; '*'; '/'; '%']

module ParserAliases =
    let str = pstring
    let isBlank = fun c -> c = ' ' || c = '\t'
    let ws = skipMany1SatisfyL isBlank "فاصله"


module SignParsers =
    open ParserAliases
    open Signs
    let peq : Parser<_> = str eq
    let sigc : Parser<_> = str ":" .>> ws
    let plight : Parser<_> = str lightArrow .>> ws
    let poPara : Parser<_> = str oPara
    let pcPara : Parser<_> = str cPara

module Keywords =
    let use' = "استفاده"
    let type' = "نوع"
    let with' = "با"
    let if' = "اگه"
    let then' = "اونوقت"
    let else' = "وگرنه"
    let true' = "درست"
    let false' = "غلط"

    let from' = "از"
    let ta' = "تا"

    let print' = "بنویس"

    let keywords = [
        use'; type'; with'; if'; then'; else'; true'; false'; print';
        from'; ta';
    ]


module KeywordParsers =
    open ParserAliases
    open Keywords
    let keyword str = pstring str //>>? nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str

    let pkw_use : Parser<_> = keyword use'
    let pkw_type : Parser<_> = keyword type'
    let pkw_with : Parser<_> = keyword with'
    let pkw_if : Parser<_> = keyword if' .>>? ws
    let pkw_then : Parser<_> = keyword then'
    let pkw_else : Parser<_> = keyword else'

    let pkw_from : Parser<_> = keyword from'
    let pkw_ta : Parser<_> = keyword ta'

    let pkw_true : Parser<_> = stringReturn true' HstTrue
    let pkw_false : Parser<_> = stringReturn false' HstFalse

    let pkw_print : Parser<_> = keyword print'

module ReserveParsers =
    open Keywords
    open ParserAliases
    let notReserved : Parser<_> =
        let strWs x = str x .>> ws
        keywords |> List.map (strWs >> notFollowedBy) |> List.reduce (>>?)


    let idf = notReserved >>? identifier (IdentifierOptions()) .>> ws <?> "شناساگر"
    let idfn = notReserved >>? identifier (IdentifierOptions()) <?> "شناساگر"

module LiteralParsers =
    open KeywordParsers
    open ParserAliases
    open ReserveParsers
    /// Parses a string literal
    let stringLiteral : Parser<_> =
        let normalCharSnippet = many1Satisfy (fun c -> c <> '/' && c <> '"')
        let specialChars = anyOf "/نرت\"" <?> "یکی از کاراکتر های ویژه «ن» یا «ر» یا «ت» انتظار میرفت"
        let escapedChar = pstring "/" >>. (specialChars |>> function
                                                                | 'ن' -> "\n"
                                                                | 'ر' -> "\r"
                                                                | 'ت' -> "\t"
                                                                | c   -> string c) .>> (pstring "/" <?> "انتهای کاراکتر ویژه باید با «/» تمام شود")
        between (pstring "\"") (pstring "\"")
                (manyStrings (normalCharSnippet <|> escapedChar)) <?> "رشته متنی"

    let numLiteral : Parser<HstNumber> = (attempt pint32 <?> "عدد صحیح" |>> Integer) <|>
                                         (attempt pfloat <?> "عدد اعشاری" |>> Float)

    let precordInstance : Parser<_> = str "رکورد"
    let pbool = (attempt pkw_true <|> attempt pkw_false) <?> "مقدار بولی"

    let punit = stringReturn "()" Unit

    let pliteral : Parser<HstValue> = (attempt stringLiteral |>> String) <|>
                                      (attempt numLiteral |>> Number) <|>
                                      (attempt pbool |>> Boolean) <|>
                                      (attempt punit)


    let poperator : Parser<HstOperator> = many1Satisfy (fun c -> Signs.availableOpChars |> List.contains c) |>> Operator


module LanguageConstructParsers =
    open SignParsers
    open KeywordParsers
    open LiteralParsers
    open ParserAliases
    open BaseTypes
    open ReserveParsers
    open IndentationParserWithBacktracking
    open ParserUtils

    let comment : Parser<_> = pstring "#" >>. skipRestOfLine false
    let wsEol = skipManySatisfy isBlank >>. optional comment
    let owsEol = optional wsEol
    let nl : Parser<_> = newline
    let indNewline = indentedMany1 nl "خط بندی جدید"

    let ptypesig : Parser<HstTypeSig> = sepBy1 idf plight  |>>
                                        fun x -> if x.Length = 1 then Atomic (TypeName x.[0])
                                                 else (x |> List.map TypeName) |> Chain




    let pexpr, pexprRef = createParserForwardedToRef<HstExpression, IndentationState>()
    let pparaExpr, pparaExprRef = createParserForwardedToRef<HstExpression, IndentationState>()
    let value = pliteral |>> Value

    let valueExpr = pliteral |>> ExpValue

    let bindingExpr = idfn |>> (HstIdentifier >> BindingExpr)

    let valOrBindExpr = attempt valueExpr <|> attempt bindingExpr

    let opCall =
        pipe3 (valOrBindExpr .>>? ws)
              (poperator .>>? ws)
              (pexpr)

              (fun exp1 op exp2 -> { op = op; exp1 = exp1; exp2 = exp2})

    let opCallExpr = opCall |>> ExpOpCall

    let paraOpCallExpr = poPara  >>? opCallExpr .>>? pcPara

    do pexprRef := achoice [
        paraOpCallExpr
        opCallExpr
        valueExpr
        bindingExpr
    ]

    let pstatement, pstatementRef = createParserForwardedToRef<HstStatement, IndentationState>()
    let indentedStatements, indentedStatementsRef = createParserForwardedToRef()



    let ifStmt =
        pipe2 (pkw_if >>? pexpr .>>? ws .>>? pkw_then .>>? wsEol)
              (indentedStatements .>>? wsEol)

              (fun cond thn -> IfStmt { cond = cond; thendo = thn }) <?> "عبارت شرطی"

    let ifStmtInline =
        pipe2 (pkw_if >>? pexpr)
              (ws >>? pkw_then >>? ws >>? pstatement)

              (fun cond thn -> IfStmt { cond = cond; thendo = [thn] }) <?> "عبارت شرطی"

    let ifElseStmt =
        pipe3 (pkw_if >>? pexpr .>>? ws .>>? pkw_then .>>? wsEol)
              (indentedStatements .>>? wsEol)
              (indented pkw_else >>? indentedStatements .>>? wsEol)

              (fun cond thn els -> IfElseStmt { cond = cond; thendo = thn; elsedo = els }) <?> "عبارت شرطی"

    let ifElseStmtInline =
        pipe3 (pkw_if >>? pexpr)
              (ws >>? pkw_then >>? ws >>? pstatement)
              (ws >>? pkw_else >>? ws >>? pstatement)

              (fun cond thn els -> IfElseStmt { cond = cond; thendo = [thn]; elsedo = [els] }) <?> "عبارت شرطی"

    let binding =
        pipe3 (many1 idf .>>? sigc)
              (ptypesig .>>? peq .>>? wsEol)
              (indentedStatements)

              (fun id tsig body -> Binding {
                   identifier = HstIdentifier id.[0]
                   args = if id.Length > 1 then Some (id.[1..] |> List.map HstIdentifier) else None
                   bindingType = tsig
                   exps = body
              })

    let basicBinding =
        pipe3 (many1 idf .>>? sigc)
              (ptypesig .>>? peq .>>? ws)
              (pstatement .>>? wsEol)

              (fun id tsig stmt -> Binding {
                  identifier = HstIdentifier id.[0]
                  args = if id.Length > 1 then Some (id.[1..] |> List.map HstIdentifier) else None
                  bindingType = tsig
                  exps = [stmt]
              })

    let ploop =
        pipe4 (pkw_from >>? ws >>? idf .>>? peq .>>? ws)
              (numLiteral .>>? ws)
              (pkw_ta >>? ws >>? numLiteral .>>? ws .>>? pkw_then .>>? wsEol)
              (indentedStatements .>>? wsEol)

              (fun idf from' to' body -> { boundIdf = HstIdentifier idf; fromIndex = from'; toIndex = to'; body = body }) |>> Loop

    let ploopInline =
        pipe4 (pkw_from >>? ws >>? idf .>>? peq .>>? ws)
              (numLiteral .>>? ws)
              (pkw_ta >>? ws >>? numLiteral .>>? ws .>>? pkw_then .>>? ws)
              (pstatement)

              (fun idf from' to' stmt -> { boundIdf = HstIdentifier idf ; fromIndex = from'; toIndex = to'; body = [stmt] }) |>> Loop


    let typedef =
        pipe2 (idf .>>? sigc)
              pkw_type

              (fun id _ -> TypeAlias (id, (TypeName "x") |> Atomic) |> TypeDef)

    let pcallArg = attempt (pliteral |>> CallValue) <|> attempt (idfn |>> (HstIdentifier >> CallIdf))

    let funcCall =  
        pipe2 idfn
              (many1 (ws >>? pcallArg))

              (fun idf callArgs -> { id = HstIdentifier idf; callArgs = callArgs }) |>> FuncCall

    let printStmt = (pkw_print >>? ws >>? pexpr) |>> PrintStatement

    do pstatementRef := achoice [
                                 ifElseStmt
                                 ifElseStmtInline
                                 ifStmt
                                 ifStmtInline
                                 value
                                 typedef
                                 binding
                                 basicBinding
                                 printStmt
                                 ploop
                                 ploopInline
                                 funcCall
                                ]

    do indentedStatementsRef := indentedMany1 pstatement "عبارت"

    let parastatement =  optional poPara >>? pstatement .>>? optional pcPara


    let document = indentedStatements .>> spaces .>> eof

open FParsec.Error

let longestMathRuleSplit (err:string) =
    err.Split([|"The parser backtracked after:"|], StringSplitOptions.None)
    |> Array.last

type CompilerAnswer() = class end

type CompilerSuccess(result) =
    inherit CompilerAnswer()
    member __.Result : HstStatement list = result

type CompilerFailure(result:string) =
    inherit CompilerAnswer()
    member __.Error = result

let test str =
    match runParserOnString LanguageConstructParsers.document (IndentationState.Create()) "" (str)  with
    | Success(result, _, _)   -> CompilerSuccess result :> CompilerAnswer
    | Failure(errorMsg, err, _) -> CompilerFailure (errorMsg |> toPersian |> longestMathRuleSplit) :> CompilerAnswer

