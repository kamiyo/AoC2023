open System
open System.IO

let readFile input = File.ReadAllLines input

let myReg =
    System.Text.RegularExpressions.Regex(@"(?<pos>[A-Z0-9]{3}) = \((?<left>[A-ZA-Z0-9]{3}), (?<right>[A-ZA-Z0-9]{3})\)")

let makeMapping (input: string) =
    let m = myReg.Match input
    m.Groups["pos"].Value, (m.Groups["left"].Value, m.Groups["right"].Value)

type PositionMap = Map<string, string * string>

let rec traverse (mapping: PositionMap) (instructions: string) (currentPos: string) (currentInstr: int) (steps: uint) =
    if currentPos = "ZZZ" then
        steps
    else
        let currMap = mapping |> Map.find (if currentPos = "" then "AAA" else currentPos)

        let newPos =
            match instructions[currentInstr] with
            | 'L' -> fst currMap
            | 'R' -> snd currMap
            | _ -> raise (Exception "Shouldn't be here")

        traverse mapping instructions newPos ((currentInstr + 1) % instructions.Length) (steps + 1u)

let rec traverseGhost
    (mapping: PositionMap)
    (instructions: string)
    (currentPos: string)
    (currentInstr: int)
    (steps: uint)
    =
    if currentPos.Chars 2 = 'Z' then
        steps
    else
        let currMap = mapping[currentPos]

        let newPos =
            match instructions[currentInstr] with
            | 'L' -> currMap |> fst
            | 'R' -> currMap |> snd
            | _ -> raise (Exception "Shouldn't be here")

        traverseGhost mapping instructions newPos ((currentInstr + 1) % instructions.Length) (steps + 1u)

let rec gcd (a: uint) (b: uint) =
    match b with
    | 0u -> a
    | _ -> gcd b (a % b)

let lcm (a: uint) (b: uint) =
    a * b / (gcd a b)

let input = readFile "./inputs/08.txt"
let instructions = input |> Array.head

let mapping =
    input
    |> Array.tail
    |> Array.tail
    |> Array.fold
        (fun state l ->
            let k, v = makeMapping l
            state |> Map.add k v)
        (Map<string, string * string>([]))

mapping
|> Map.filter (fun k v -> k.Chars 2 = 'A')
|> Map.keys
|> List.ofSeq
|> List.map (fun k -> traverseGhost mapping instructions k 0 0u)
|> List.reduce (fun l r -> l * r)
|> printfn "%A"
