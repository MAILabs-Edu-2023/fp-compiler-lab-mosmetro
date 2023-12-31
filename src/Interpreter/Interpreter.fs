﻿namespace Interpreter

open System
open System.IO
open Parser

module Interpreter = 

    type MObject = 
        | MBool of bool
        | MNumber of float
        | MString of string
        | MList of MObject list
        | MSeq of left:float * step:float * right:float

    let rec private interpretedTypeToString = function
        | MBool(b) -> if b then "True" else "False"
        | MNumber(n) -> n.ToString()
        | MString(s) -> s
        | MList(l) -> "(" + (l |> List.map (fun el -> interpretedTypeToString el) |> String.concat " ") + ")"
        | MSeq (left, step, right) ->
            let seqValues =
                seq { for i in left .. step .. right do yield i }
                |> Seq.map (fun v -> v.ToString())
                |> String.concat " "
            "{ " + seqValues + " }"

    type StateObject =
        | VariableState of value: MObject
        | FunctionState of args: string list * ast: Ast * capturedState: Map<string, StateObject>

    // Convert AstList<AstVariable> to string list.
    let rec private processFunArgs = fun lst ->
        let varToStr = fun var ->
            match var with
                | AstVariable(v) -> v
                | _ -> failwith "AstVariable expected in fun variable declaration list"

        match lst with
            | AstList(ls) -> ls |> List.map varToStr
            | _ -> failwith "AstList expected for variable declaration"

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
    let rec private eval exp stateEnv =
        match exp with
        | AstBool(b) -> MBool(b)
        | AstNumber(n) -> MNumber(n)
        | AstString(s) -> MString(s)

        | AstVariable(x) ->
            (match Map.tryFind x stateEnv with
             | Some(value) -> 
                match value with
                    | VariableState(v) -> v
                    | FunctionState(_) -> executeFun value (MList []) stateEnv
             | None -> failwith "Variable or function not found")

        | AstList(lst) ->
            (
                match lst with
                | AstKeyword("let") :: varName :: rawOperators :: innerCodeArea ->
                    let value = eval rawOperators stateEnv
                    
                    match varName with
                        | AstVariable(varName) -> eval (AstList innerCodeArea) (Map.add varName (VariableState value) stateEnv)
                        | _ -> failwith "Variable name must be AstVariable"

                | AstKeyword("defun") :: funName :: funVariables :: funcitonBody :: innerCodeArea ->
                    let variables = processFunArgs funVariables

                    match funName with
                        | AstVariable(funName) -> eval (AstList innerCodeArea) (Map.add funName (FunctionState(variables, funcitonBody, stateEnv)) stateEnv)
                        | _ -> failwith "Function name name must be AstVariable"

                | AstKeyword("if") :: condition :: trueBranch :: falseBranch :: [] ->
                    let evalCondition = eval condition stateEnv
                    
                    if evalCondition = MBool(true) then 
                        eval trueBranch stateEnv
                    else 
                        eval falseBranch stateEnv

                | AstKeyword("list") :: expressions ->
                    match expressions with
                    | AstVariable(command) :: listName :: [] when command = "head" || command = "tail" ->
                        let objectFromStorage = eval listName stateEnv
                        match objectFromStorage with
                        | MList(list) ->
                            if command = "head" then list.Head
                            else MList(list.Tail)
                        | _ -> failwith "Incorrect variable, list expected"
                    | AstVariable(command) :: nth :: listName :: [] when command = "item" ->
                        let objectFromStorage = eval listName stateEnv
                        match objectFromStorage with
                        | MList(list) ->
                            match eval nth stateEnv with
                            | MNumber(index) ->
                                if (list.Length > int index) then list.Item (index |> int)
                                else failwith "Index out of bounds"
                            | _ -> failwith "Incorrect index, number expected"
                        | _ -> failwith "Incorrect variable, list expected"
                    | _ -> let rec evalList expressions stateEnv acc =
                               match expressions with
                               | [] -> MList(acc)
                               | exp :: rest ->
                                   let evaluated = eval exp stateEnv
                                   evalList rest stateEnv (acc @ [evaluated]) // Adding every calculated value to accumulator.

                           evalList expressions stateEnv []
                           
                | AstKeyword("seq") :: parameters ->
                    match parameters with
                    | AstVariable(command) :: seqName :: [] when command = "head" || command = "tail" ->
                        let objectFromStorage = eval seqName stateEnv
                        match objectFromStorage with
                        | MSeq(left, step, right) ->
                            if command = "head" then MNumber(left)
                            else MSeq(left + step, step, right)
                        | _ -> failwith "Incorrect variable, sequence expected"
                    | AstVariable(command) :: nth :: seqName :: [] when command = "item" ->
                        let objectFromStorage = eval seqName stateEnv
                        match objectFromStorage with
                        | MSeq(left, step, right) ->
                            match eval nth stateEnv with
                            | MNumber(index) ->
                                if left + index * step <= right then MNumber(left + index * step)
                                else failwith "Index out of bounds"
                            | _ -> failwith "Incorrect index, number expected"
                        | _ -> failwith "Incorrect variable, list expected"
                    | AstNumber(left) :: AstVariable("..") :: AstNumber(right) :: [] ->
                        MSeq(left, 1, right)
                    | AstNumber(left) :: AstVariable("..") :: AstNumber(step) :: AstVariable("..") :: AstNumber(right) :: [] ->
                        MSeq(left, step, right)
                    | _ -> failwith "Incorrect parameters provided"
                    
                | AstKeyword("file") :: parameters ->
                    match parameters with
                    | AstVariable(command) :: path :: [] when command = "create" ->
                        match eval path stateEnv with
                        | MString(stringPath) ->
                            let fullPath =
                                    Path.Combine(
                                        Path.GetPathRoot(Environment.SystemDirectory),
                                        stringPath)
                                    
                            File.AppendAllText(fullPath, String.Empty)
                            MString("File successfully created\n")
                        | _ -> failwith "Undefined behaviour"
                    | AstVariable(command) :: path :: [] when command = "read" ->
                        match eval path stateEnv with
                        | MString(stringPath) ->
                            try
                                let fullPath =
                                    Path.Combine(
                                        Path.GetPathRoot(Environment.SystemDirectory),
                                        stringPath)

                                let internals = File.ReadAllText(fullPath)
                                MString(internals)
                            with
                            | ex -> failwith ("Problem occured: " + ex.Message)
                        | _ -> failwith "Undefined behaviour"
                    | AstVariable(command) :: path :: varName :: [] when command = "write" ->
                        match eval path stateEnv with
                        | MString(stringPath) ->
                            try
                                let fullPath =
                                    Path.Combine(
                                        Path.GetPathRoot(Environment.SystemDirectory),
                                        stringPath)
                                    
                                match varName with
                                | AstVariable _ ->
                                    let actualText = eval varName stateEnv
                                    match actualText with
                                    | MString(s) ->
                                        File.AppendAllText(fullPath, s)
                                        MString("Lines successfully appended\n")
                                    | _ -> failwith "Undefined behaviour"
                                | AstString(s) ->
                                    File.AppendAllText(fullPath, s)
                                    MString("Lines successfully appended\n")
                                | _ -> failwith "Undefined behaviour"
                            with
                            | ex -> failwith ("Problem occured: " + ex.Message)
                        | _ -> failwith "Undefined behaviour"
                    | _ -> failwith "Incorrect parameters provided"

                | AstVariable(operation) :: elements ->
                    if elements.IsEmpty then
                        eval (AstVariable operation) stateEnv
                    else
                        let astResults = List.map (fun item -> eval item stateEnv) elements

                        match Map.tryFind operation stateEnv with
                        | Some(value) -> 
                            match value with
                                | FunctionState(_) -> executeFun value (MList astResults) stateEnv
                                | _ -> failwith "Undefined behaviour"
                        | None -> funof operation astResults

                | _ -> 
                    if lst.Length = 1 then
                        eval lst.Head stateEnv
                    else
                        MList(List.map (fun item -> eval item stateEnv) lst)
            )

        | _ -> failwith "Undefined behaviour"

    and private executeFun funObj args stateEnv = 
        let rec zip (a:'a list , b:'b list) : list<'a * 'b> =
            if List.length a = 1 then [List.head a , List.head b]
            elif List.length a = 0 then []
            else (List.head a, List.head b) :: zip (List.tail a , List.tail b) 

        let rec insertArgs (args: (string * MObject) list, state: Map<string, StateObject>) = 
            if args.IsEmpty then
                state
            else
                let (st, obj) = args.Head

                insertArgs (args.Tail, Map.add st (VariableState obj) state)
            
        match funObj with
        | FunctionState(funArgs, funAst, capturedState) ->
            match args with
            | MList(args) ->
                let mergedState = Map.fold (fun acc key value -> Map.add key value acc) capturedState stateEnv

                if args.Length <> funArgs.Length then
                    failwith "Incorrect number of function arguments"
                else
                    let evalState = insertArgs (zip (funArgs, args), mergedState)
                    eval funAst evalState
            | _ -> failwith "Undefined Behaviour"
        | _ ->
            failwith "Undefined Behaviour"

    let public Launch = fun tree ->
        let evalRes = eval tree Map.empty

        interpretedTypeToString evalRes