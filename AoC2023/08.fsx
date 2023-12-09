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

let detectCycleBrent (mapping: PositionMap) (instructions: string) (initial: string) =
    let mutable power = 1L
    let mutable lambda = 1L
    let mutable hareInstr = 0
    let mutable tortoise = initial, 0
    
    let next ((currentPos, instrIdx): string * int) =
        let currMap = mapping[currentPos]        
        match instructions[instrIdx] with
        | 'L' -> currMap |> fst, (instrIdx + 1) % instructions.Length
        | 'R' -> currMap |> snd, (instrIdx + 1) % instructions.Length
        | _ -> raise (Exception "Shouldn't be here")
    
    let mutable hare = initial, 0
    hare <- next hare
        
    while tortoise <> hare do
        if power = lambda then
            tortoise <- hare
            power <- power * 2L
            lambda <- 0
        hare <- next hare
        lambda <- lambda + 1L
        
    tortoise <- initial, 0
    hare <- initial, 0
    hareInstr <- 0
    for _ in 0L .. lambda - 1L do
        hare <- next hare
    
    let mutable mu = 0L
    while tortoise <> hare do
        tortoise <- next tortoise
        hare <- next hare
        mu <- mu + 1L

    initial, lambda, mu

let iterMap (mapping: PositionMap) (instructions: string) ((currentPos, instrIdx): string * int) =
    let currMap = mapping[currentPos]        
    match instructions[instrIdx] with
    | 'L' -> currMap |> fst, (instrIdx + 1) % instructions.Length
    | 'R' -> currMap |> snd, (instrIdx + 1) % instructions.Length
    | _ -> raise (Exception "Shouldn't be here")

let rec gcd (a: uint64) (b: uint64) =
    match b with
    | 0UL -> a
    | _ -> gcd b (a % b)

let lcm (a: uint64) (b: uint64) =
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

let starts =
    mapping
    |> Map.filter (fun k v -> k.Chars 2 = 'A')
    |> Map.keys
    |> List.ofSeq
let cycles = 
    starts
    // |> List.map (fun k -> traverseGhost mapping instructions k 0 0u)
    |> List.map (fun k -> detectCycleBrent mapping instructions k)
    // |> List.reduce (fun l r -> l * r)
    |> printfn "%A"

let rec findEndPos (mapping: PositionMap) (instructions: string) ((currentPos, instrIdx): string * int) (count: int) =
    match currentPos.Chars 2 = 'Z' with
    | true -> count
    | false -> findEndPos mapping instructions (iterMap mapping instructions (currentPos, instrIdx)) (count + 1)

let endPos =
    starts
    |> List.map (fun s -> findEndPos mapping instructions (s, 0) 0)
    |> List.map uint64
    |> List.reduce lcm
printfn "%A" endPos

// let test =
//     starts
//     |> List.map (fun s ->
//         let mutable st = (s, 0)
//         for _ in 0 .. 1977812634 do
//             st <- iterMap mapping instructions st
//         st)
// let mutable aaa = ("AAA", 0)
// for _ in 0 .. 1 do
//     aaa <- iterMap mapping instructions aaa
// for _ in 0 .. 1977812635 do
//     aaa <- iterMap mapping instructions aaa
// printfn "%A" test

// 2 + (x * 18113) + 18111