module Tests

open Xunit
open Parser
open Interpreter

[<Fact>]
let ``Testing of easy calculation`` () =
    let test =  Parser.ParseString("(+ 2.5 2)")
    let result = Interpreter.launch test
    Assert.Equal(result |> string, "4.5")

[<Fact>]
let ``Testing of difficult calculation`` () =
    let test =  Parser.ParseString("(/ (* (+ 10 15) (- 7 3)) 2.5)")
    let result = Interpreter.launch test
    Assert.Equal(result |> string, "40")

[<Fact>]
let ``Testing of logical and mathematical operators`` () =
    let test =  Parser.ParseString("(or (and (= (+ (+ 1 2) (- 8 6)) 5) true) false)")
    let result = Interpreter.launch test
    Assert.Equal(result |> string, "True")

[<Fact>]
let ``Testing of list output`` () =
    let test =  Parser.ParseString("( 1 2 3 4 5 )")
    let result = Interpreter.launch test
    Assert.Equal(result |> string, "( 1 2 3 4 5 )")