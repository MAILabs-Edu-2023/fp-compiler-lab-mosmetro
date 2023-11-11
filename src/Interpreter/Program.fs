open System
open Parser

[<EntryPoint>]
let main args =
    let parsedString = Parser.ParseString("(defun (fibonacci) (N)
    (if (or (zerop N) (= N 1))
        1
        (+ fibonacci (- N 1 F2) fibonacci (- N 2))))


(print (fibonacci 5))")

    match parsedString with
    | Result.Ok(res) -> printfn "%A" res
    | Result.Error(err) -> printfn "%s" err

    0