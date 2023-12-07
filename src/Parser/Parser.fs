namespace Parser

open FParsec

// Simple FParsec-based parser
// Not the most obious code (lib has a very rich API), but it's quite elegant and functional

type Ast = 
    | AstBool of bool // true/false
    | AstNumber of float // 42/6.9
    | AstString of string // "string"
    | AstKeyword of string // defun, let
    | AstVariable of string // fibonacchi/N/wtf - everything that is not a string or a keyword
    | AstList of List<Ast> // Not an actual list as a data structure, just AST abstaction



module Parser =
    // For parser tracing
    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A (%c %c): Entering %s" stream.Position (stream.Peek2().Char0) (stream.Peek2().Char1) label
            let reply = p stream
            printfn "%A (%c %c): Leaving %s (%A)" stream.Position (stream.Peek2().Char0) (stream.Peek2().Char1) label reply.Status
            reply


    let private astBoolTrue = stringReturn "true" <| AstBool true .>> spaces
    let private astBoolFalse = stringReturn "false" <| AstBool false .>> spaces
    let private astBool = astBoolFalse <|> astBoolTrue

    let private astNumber =  pfloat .>> spaces |>> AstNumber

    let private manyCharsBetween popen pclose pchar = popen >>? manyCharsTill pchar pclose
    let private anyStringBetween popen pclose = manyCharsBetween popen pclose anyChar
    let private quotedString = skipChar '"' |> anyStringBetween <| skipChar '"'

    let private astString = quotedString .>> spaces |>> AstString

    let private astKeywordDefun = stringReturn "defun" <| AstKeyword "defun" .>> spaces
    let private astKeywordLet = stringReturn "let" <| AstKeyword "let" .>> spaces
    let private astKeywordList = stringReturn "list" <| AstKeyword "list" .>> spaces

    let private astKeywordIf = stringReturn "if" <| AstKeyword "if" .>> spaces

    let private astKeyword = astKeywordDefun <|> astKeywordLet <|> astKeywordList <|> astKeywordIf

    let private astVariable = many1Chars (noneOf "\"\\ ()\n") .>> spaces |>> AstVariable

    let astOper, astOperRef = createParserForwardedToRef()

    let private astList = 
        skipChar '(' >>. spaces >>.
        (attempt (sepBy astOper spaces) <|> sepEndBy1 astOper spaces)
        .>> spaces .>> skipChar ')' .>> spaces
        |>> AstList

    astOperRef.Value <- 
        choice [
            astBool
            astNumber
            astString
            attempt astList
            attempt astKeyword
            astVariable
        ]

    let private astFullParser = spaces >>. many astOper .>> eof |>> AstList

    let ParseString (s: string): Result<Ast, string> =
        match run astFullParser s with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) ->Result.Error err