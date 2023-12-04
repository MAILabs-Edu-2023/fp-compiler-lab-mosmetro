namespace Interpreter

open Parser

module Interpreter = 

    // Operators translation.
    let private funof = function
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

    // Ability to cast all types to string for console output.
    let rec public AstToString = function
    | AstBool(b) -> string b
    | AstNumber(n) -> string n
    | AstList(l) -> "(" + (List.fold (fun acc item -> acc + AstToString item + " ") " " l) + ")"
    | AstString(s) | AstKeyword(s) | AstVariable(s) -> s  // Already string types.

    // Main interpreter logic.
    let rec private eval exp env =
        match exp with
        | AstBool(b) -> AstBool(b)
        | AstNumber(n) -> AstNumber(n)
        | AstString(s) -> AstString(s)
        | AstVariable(x) ->
            (match Map.tryFind x env with
             | Some(value) -> value
             | None -> failwith "Variable not found")
        | AstList(lst) ->
            (match lst with
             | AstKeyword("let") :: varName :: rawOperators :: innerCodeArea ->
                let value = eval rawOperators env
                eval (AstList innerCodeArea) (Map.add (AstToString varName) value env)
             | AstVariable(operation) :: elements ->
                if elements.IsEmpty then
                    eval (AstVariable operation) env
                else
                    let astResults = List.map (fun item -> eval item env) elements
                    funof operation astResults
             | AstKeyword("if") :: condition :: trueBranch :: falseBranch :: [] ->
                 let evalCondition = eval condition env
                 if evalCondition = AstBool(true) then eval trueBranch env
                 else eval falseBranch env
             | _ -> AstList(List.map (fun item -> eval item env) lst))
        | _ -> failwith "Undefined behaviour"

    let public launch = function
        | Result.Ok(tree) ->
            match eval tree Map.empty with
            | AstList(lst) -> AstToString lst.Head
            | _ -> failwith "Unexpected type of interpretation result"
        | Result.Error(err) -> failwith err