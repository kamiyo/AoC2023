open System
open System.IO

let readFile input = File.ReadAllLines input

let rec getNextInSequence (history: int list) =
    match history |> List.distinct |> List.length = 1 with
    | true -> history |> List.last
    | false ->
        let diffs =
            List.map2 (fun l r -> l - r) (history |> List.tail) (history |> List.take ((history |> List.length) - 1))

        (history |> List.last) + (getNextInSequence diffs)
        
let rec getPrevInSequence (history: int list) =
    match history |> List.distinct |> List.length = 1 with
    | true -> history |> List.head
    | false ->
        let diffs =
            List.map2 (fun l r -> l - r) (history |> List.tail) (history |> List.take ((history |> List.length) - 1))
        let next = getPrevInSequence diffs
        (history |> List.head) - next

"./inputs/09.txt"
|> readFile
|> Array.map (fun l -> l.Split(" ") |> Array.map int |> List.ofArray)
|> Array.map getPrevInSequence
|> Array.reduce (fun l r -> l + r)
|> printfn "%A"