namespace Interpreter

open Parser

module Interpreter = 

    type IntepretedType = 
        | MBool of bool
        | MNumber of float
        | MString of string
        | MList of IntepretedType list

    let rec private interpretedTypeToString = function
        | MBool(b) -> if b then "True" else "False"
        | MNumber(n) -> n.ToString()
        | MString(s) -> s
        | MList(l) -> "(" + (l |> List.map(fun el -> interpretedTypeToString el) |> String.concat " ")  + ")"

    type Variable = {value: IntepretedType}
    type Function = {args: string list; ast: Parser.Ast}

    // Operators translation.
    let private funof = function
        | "+" -> (function 
                    | [MNumber(a); MNumber(b)] -> MNumber(a + b)
                    | [MString(a); MString(b)] -> MString(a + b)
                    | [MNumber(a); MString(b)] -> MString(string a + b)
                    | [MString(a); MNumber(b)] -> MString(a + string b)
                    | _ -> failwith "Invalid arguments for addition")
        | "-" -> (function 
                    | [MNumber(a); MNumber(b)] -> MNumber(a - b)
                    | _ -> failwith "Invalid arguments for subtraction")
        | "*" -> (function 
                    | [MNumber(a); MNumber(b)] -> MNumber(a * b)
                    | _ -> failwith "Invalid arguments for multiplication")
        | "/" -> (function 
                    | [MNumber(a); MNumber(b)] -> MNumber(a / b)
                    | _ -> failwith "Invalid arguments for division")
        | "=" -> (function 
                    | [MNumber(a); MNumber(b)] -> if a = b then MBool(true) else MBool(false)
                    | _ -> failwith "Invalid arguments for equality check")
        | ">" -> (function 
                    | [MNumber(a); MNumber(b)] -> if a > b then MBool(true) else MBool(false)
                    | _ -> failwith "Invalid arguments for greater than check")
        | "<" -> (function 
                    | [MNumber(a); MNumber(b)] -> if a < b then MBool(true) else MBool(false)
                    | _ -> failwith "Invalid arguments for less than check")
        | "<=" -> (function 
                    | [MNumber(a); MNumber(b)] -> if a <= b then MBool(true) else MBool(false)
                    | _ -> failwith "Invalid arguments for less than or equal to check")
        | ">=" -> (function 
                    | [MNumber(a); MNumber(b)] -> if a >= b then MBool(true) else MBool(false)
                    | _ -> failwith "Invalid arguments for greater than or equal to check")
        | "or" -> (function 
                    | [MBool(a); MBool(b)] -> if (a || b) then MBool(true) else MBool(false)
                    | _ -> failwith "Invalid arguments for or to check")
        | "and" -> (function 
                    | [MBool(a); MBool(b)] -> if (a && b) then MBool(true) else MBool(false)
                    | _ -> failwith "Invalid arguments for and to check")
        | _ -> failwith "Unsupported operator was given"

    // Main interpreter logic.
    let rec private eval exp variableEnv =
        match exp with
        | AstBool(b) -> MBool(b)
        | AstNumber(n) -> MNumber(n)
        | AstString(s) -> MString(s)

        | AstVariable(x) ->
            (match Map.tryFind x variableEnv with
             | Some(value) -> value
             | None -> failwith "Variable not found")

        | AstList(lst) ->
            (
                match lst with
                | AstKeyword("let") :: varName :: rawOperators :: innerCodeArea ->
                    let value = eval rawOperators variableEnv
                    
                    match varName with
                        | AstVariable(varName) -> eval (AstList innerCodeArea) (Map.add varName value variableEnv)
                        | _ -> failwith "Variable name must be AstVariable"

                | AstKeyword("if") :: condition :: trueBranch :: falseBranch :: [] ->
                    let evalCondition = eval condition variableEnv
                    
                    if evalCondition = MBool(true) then 
                        eval trueBranch variableEnv
                    else 
                        eval falseBranch variableEnv

                | AstVariable(operation) :: elements ->
                    if elements.IsEmpty then
                        eval (AstVariable operation) variableEnv
                    else
                        let astResults = List.map (fun item -> eval item variableEnv) elements
                        funof operation astResults

                | _ -> MList(List.map (fun item -> eval item variableEnv) lst)
            )

        | _ -> failwith "Undefined behaviour"

    let public Launch = fun tree ->
        let evalRes = eval tree Map.empty

        interpretedTypeToString evalRes