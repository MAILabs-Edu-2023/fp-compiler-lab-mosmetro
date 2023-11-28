open System
open Parser

module Interpreter = 
    let private funof = function
        | "+" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> AstNumber(a + b)
                    | _ -> failwith "Invalid arguments for addition")
        | "-" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> AstNumber(b - a)
                    | _ -> failwith "Invalid arguments for subtraction")
        | "*" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> AstNumber(a * b)
                    | _ -> failwith "Invalid arguments for multiplication")
        | "/" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> AstNumber(b / a)
                    | _ -> failwith "Invalid arguments for division")
        | "=" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> if a = b then AstBool(true) else AstBool(false)
                    | _ -> failwith "Invalid arguments for equality check")
        | ">" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> if b > a then AstBool(true) else AstBool(false)
                    | _ -> failwith "Invalid arguments for greater than check")
        | "<" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> if b < a then AstBool(true) else AstBool(false)
                    | _ -> failwith "Invalid arguments for less than check")
        | "<=" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> if b <= a then AstBool(true) else AstBool(false)
                    | _ -> failwith "Invalid arguments for less than or equal to check")
        | ">=" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> if b >= a then AstBool(true) else AstBool(false)
                    | _ -> failwith "Invalid arguments for greater than or equal to check")
        | _ -> failwith "Unsupported operator was given"

    let rec private eval exp env =
        match exp with
        | AstBool(b) -> AstBool(b)
        | AstNumber(n) -> AstNumber(n)
        | AstString(s) -> AstString(s)
        | AstKeyword(x) -> Map.find x env
        | AstVariable(v) -> AstVariable(v)  // TODO: реализовать вариацию случаев (операторы, названия функций, переменных)
        | AstList(l) -> AstList(List.map (fun item -> eval item env) l)



    let launch = function
        | Result.Ok(tree) -> eval tree Map.empty
        | Result.Error(err) -> failwith err


[<EntryPoint>]
let main _ =
    let parsedString = Parser.ParseString("(defun (fibonacci) (N)
    (if (or (zerop N) (= N 1))
        1
        (+ fibonacci (- N 1 F2) fibonacci (- N 2))))


(print (fibonacci 5))")

    Interpreter.launch parsedString |> ignore

    0