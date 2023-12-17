open System
open System.IO

let HASH (input: string) =
    let mutable curr = 0

    for c in input.ToCharArray() do
        curr <- curr + (c |> int)
        curr <- curr * 17
        curr <- curr % 256

    curr

type Operation =
    | Push of int * string * int
    | Pop of int * string

let getInstruction (input: string) =
    if input |> String.exists ((=) '=') then
        let split = input.Split('=')
        let label = split[0]
        let box = HASH label
        let lens = split[1] |> int
        Push(box, label, lens)
    else
        let split = input.Split('-')
        let label = split[0]
        let box = HASH label
        Pop(box, label)

let boxes: (string * int) list array = Array.create 256 []

let hashmap =
    "./inputs/15.txt"
    |> File.ReadAllLines
    |> Array.head
    |> (fun s -> s.Split(','))
    |> Array.map getInstruction
    |> Array.iter (fun op ->
        match op with
        | Push(box, label, lens) ->
            match boxes[box] |> List.tryFindIndex (fun (l, _) -> l = label) with
            | Some(idx) ->
                boxes[box] <- List.updateAt idx (label, lens) boxes[box]
            | None ->
                boxes[box] <- List.append boxes[box] [(label, lens)]
        | Pop(box, label) -> boxes[box] <- boxes[box] |> List.filter (fun (b, _) -> b <> label))

printfn "%A" boxes

boxes
|> Array.indexed
|> Array.fold
    (fun state (idx, lenses) ->
        state
        + (lenses
           |> List.indexed
           |> List.fold (fun boxState (idl, (_, fp)) -> boxState + ((idx + 1) * (idl + 1) * fp)) 0))
    0
|> printfn "%A"