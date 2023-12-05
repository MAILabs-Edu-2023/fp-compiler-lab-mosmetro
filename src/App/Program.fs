open System
open System.IO

open Parser
open Interpreter

[<EntryPoint>]
let main args =
    if args.Length <> 1 then
        printf "Incorrect args!"
        1
    else
        let text = File.ReadAllText(args[0])

        match Parser.ParseString text with
        | Ok ast -> 
            let interpreted = Interpreter.Launch ast
            printfn "%s" interpreted
            0
        | Error err ->
            printfn "%s" err
            1
