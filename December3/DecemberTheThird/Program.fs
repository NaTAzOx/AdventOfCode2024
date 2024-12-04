open System.IO

let readFile path =
    let content = File.ReadAllText path
    content


let findPattern content =
    let pattern = @"(do\(\)|don't\(\)|mul\(\d+,\d+\))"
    let matches = System.Text.RegularExpressions.Regex.Matches(content, pattern)
    for m in matches do
        printfn "Match: %s" m.Value
    matches

[<EntryPoint>]
let main args =
    let filePath = @"C:\Users\NaTAzOx\Downloads\input3.txt"
    let content = readFile filePath
    let mutable result = 0
    let mutable shouldMultiply = true
    if content <> "" then
        let matches = findPattern content
        for m in matches do
            match m.Value with
            | "do()" -> shouldMultiply <- true
            | "don't()" -> shouldMultiply <- false
            | _ when m.Value.StartsWith("mul") && shouldMultiply ->
            if shouldMultiply then
                let splitted = m.Value.Split(',')
                let num1 = int (splitted.[0].Replace("mul(", ""))
                let num2 = int (splitted.[1].Replace(")", ""))
                result <- result + num1 * num2
            | _ -> ()
        printfn "Result: %d" result
    0