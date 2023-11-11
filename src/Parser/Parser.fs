namespace Parser

open FParsec

type Ast = 
    | AstBool of bool // true/false
    | AstNumber of float // 42/6.9
    | AstString of string // "string"
    | AstKeyword of string // defun, let
    | AstVariable of string // fibonacchi/N/wtf - everything that is not a string or a keyword
    | AstOper of List<Ast>



module Parser =
    let private astBoolTrue = stringReturn "true" <| AstBool true .>> spaces
    let private astBoolFalse = stringReturn "false" <| AstBool true .>> spaces
    let private astBool = astBoolFalse <|> astBoolTrue

    let private astNumber =  pfloat .>> spaces |>> AstNumber

    let private manyCharsBetween popen pclose pchar = popen >>? manyCharsTill pchar pclose
    let private anyStringBetween popen pclose = manyCharsBetween popen pclose anyChar
    let private quotedString = skipChar '"' |> anyStringBetween <| skipChar '"'

    let private astString = quotedString .>> spaces |>> AstString

    let private astKeywordDefun = stringReturn "defun" <| AstKeyword "defun" .>> spaces
    let private astKeywordLet = stringReturn "let" <| AstKeyword "let" .>> spaces

    let private astKeyword = astKeywordDefun <|> astKeywordLet

    let private astVariable = (skipChar ' ' |> anyStringBetween <| skipChar ' ') .>> spaces |>> AstVariable

    let private astLiterals =
        choice [
            astBool
            astNumber
            astString
            attempt astKeyword
            astVariable
        ]

    let astOper, astOperRef = createParserForwardedToRef()

    let private manyContained popen pclose psep p = between popen pclose <| sepBy p psep

    do astOperRef := 
        choice [
            attempt astLiterals
            astOper
        ]
        |> manyContained
            (skipChar '(' .>> spaces)
                    (skipChar ')' .>> spaces)
                    (skipChar ' ' .>> spaces)
        |>> AstOper

    let private astFullParser = spaces >>. many astOper .>> eof |>> AstOper

    let ParseString (s: string): Result<Ast, string> =
        match run astFullParser s with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) ->Result.Error err