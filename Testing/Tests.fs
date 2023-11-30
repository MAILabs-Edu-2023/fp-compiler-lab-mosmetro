module Tests

open Xunit
open Parser
open Interpreter

[<Fact>]
let ``Testing of easy calculation`` () =
    let test =  Parser.ParseString("(+ 2 2)")
    let result = Interpreter.launch test
    Assert.Equal(result |> string, "AstList [AstNumber 4.0]")

[<Fact>]
let ``Testing of difficult calculation`` () =
    let test =  Parser.ParseString("(/ (* (+ 10 15) (- 7 3)) 2.5)")
    let result = Interpreter.launch test
    Assert.Equal(result |> string, "AstList [AstNumber 40.0]")

[<Fact>]
let ``Testing of logical and mathematical operators`` () =
    let test =  Parser.ParseString("(or (and (= (+ (+ 1 2) (- 8 6)) 5) true) false)")
    let result = Interpreter.launch test
    Assert.Equal(result |> string, "AstList [AstBool true]")