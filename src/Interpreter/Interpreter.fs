namespace Interpreter

open Parser

module Interpreter = 

    // Описание операций
    let public funof = function
        | "+" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> AstNumber(a + b)
                    | [AstString(a); AstString(b)] -> AstString(a + b)
                    | [AstNumber(a); AstString(b)] -> AstString(string a + b)
                    | [AstString(a); AstNumber(b)] -> AstString(a + string b)
                    | _ -> failwith "Invalid arguments for addition")
        | "-" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> AstNumber(a - b)
                    | _ -> failwith "Invalid arguments for subtraction")
        | "*" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> AstNumber(a * b)
                    | _ -> failwith "Invalid arguments for multiplication")
        | "/" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> AstNumber(a / b)
                    | _ -> failwith "Invalid arguments for division")
        | "=" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> if a = b then AstBool(true) else AstBool(false)
                    | _ -> failwith "Invalid arguments for equality check")
        | ">" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> if a > b then AstBool(true) else AstBool(false)
                    | _ -> failwith "Invalid arguments for greater than check")
        | "<" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> if a < b then AstBool(true) else AstBool(false)
                    | _ -> failwith "Invalid arguments for less than check")
        | "<=" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> if a <= b then AstBool(true) else AstBool(false)
                    | _ -> failwith "Invalid arguments for less than or equal to check")
        | ">=" -> (function 
                    | [AstNumber(a); AstNumber(b)] -> if a >= b then AstBool(true) else AstBool(false)
                    | _ -> failwith "Invalid arguments for greater than or equal to check")
        | "or" -> (function 
                    | [AstBool(a); AstBool(b)] -> if (a || b) then AstBool(true) else AstBool(false)
                    | _ -> failwith "Invalid arguments for or to check")
        | "and" -> (function 
                    | [AstBool(a); AstBool(b)] -> if (a && b) then AstBool(true) else AstBool(false)
                    | _ -> failwith "Invalid arguments for and to check")
        | _ -> failwith "Unsupported operator was given"

    // Основная логика программы
    let rec private eval exp env =
        match exp with
        | AstBool(b) -> AstBool(b)
        | AstNumber(n) -> AstNumber(n)
        | AstString(s) -> AstString(s)
        | AstKeyword(v) ->
            (match v with
             | "defun" -> AstKeyword(v) // Implement additional logic
             | "let" -> AstKeyword(v)
             | _ -> failwith <| sprintf "Unsupported keyword '%s'" v)
        | AstVariable(x) ->
            (match Map.tryFind x env with
             | Some(value) -> value
             | None -> AstVariable(x)) // Implement additional logic
        | AstList(lst) ->
            (match lst with
             | AstVariable(operation) :: elements ->
                let astResults = List.map (fun item -> eval item env) elements
                funof operation astResults
             | AstKeyword("if") :: condition :: trueBranch :: falseBranch :: [] ->
                 let evalCondition = eval condition env
                 if evalCondition = AstBool(true) then eval trueBranch env
                 else eval falseBranch env
             | _ -> 
                AstList(List.map (fun item -> eval item env) lst))

    let launch = function
        | Result.Ok(tree) -> eval tree Map.empty
        | Result.Error(err) -> failwith err