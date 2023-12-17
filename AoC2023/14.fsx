open System
open System.IO

let mutable dim = 0

let mapping =
    "inputs/14.txt"
    |> File.ReadAllLines
    |> Array.mapi (fun row line ->
        if dim = 0 then dim <- line.Length else ()
        line.ToCharArray() |> Array.mapi (fun column c -> (row, column), c))
    |> Array.concat

let rocks = mapping |> Array.filter (fun (_, c) -> c = 'O') |> Array.map fst

let cubes = mapping |> Array.filter (fun (_, c) -> c = '#') |> Array.map fst

type Directions =
    | North
    | West
    | South
    | East

printfn "%A" (Array.groupBy snd rocks)

let roll (direction: Directions) (cubes: (int * int) array) (rocks: (int * int) array) =
    match direction with
    | North ->
        let grouped = rocks |> Array.groupBy snd

        grouped
        |> Array.map (fun (col, coords) ->
            let cubes' =
                cubes |> Array.filter (fun c -> c |> snd = col) |> Array.map fst |> Array.sort

            let mutable result = [||]

            for cIdx in -1 .. (cubes'.Length - 1) do
                let base', newRocks =
                    if (cubes' |> Array.isEmpty) then
                        -1, coords |> Array.length
                    elif cIdx = -1 then
                        -1, (coords |> Array.filter (fun (r, _) -> r < cubes'[0]) |> Array.length)
                    elif cIdx = cubes'.Length - 1 then
                        cubes'[cIdx], (coords |> Array.filter (fun (r, _) -> r > cubes'[cIdx]) |> Array.length)
                    else
                        cubes'[cIdx],
                        (coords
                         |> Array.filter (fun (r, _) -> r > cubes'[cIdx] && r < cubes'[cIdx + 1])
                         |> Array.length)

                result <-
                    Array.append
                        [| for r in 1..newRocks do
                               (base' + r, col) |]
                        result

            result)
    | West ->
        let grouped = rocks |> Array.groupBy fst

        grouped
        |> Array.map (fun (row, coords) ->
            let cubes' =
                cubes |> Array.filter (fun c -> c |> fst = row) |> Array.map snd |> Array.sort

            let mutable result = [||]

            for cIdx in -1 .. (cubes'.Length - 1) do
                let base', newRocks =
                    if (cubes' |> Array.isEmpty) then
                        -1, coords |> Array.length
                    elif cIdx = -1 then
                        -1, (coords |> Array.filter (fun (_, c) -> c < cubes'[0]) |> Array.length)
                    elif cIdx = cubes'.Length - 1 then
                        cubes'[cIdx], (coords |> Array.filter (fun (_, c) -> c > cubes'[cIdx]) |> Array.length)
                    else
                        cubes'[cIdx],
                        (coords
                         |> Array.filter (fun (_, c) -> c > cubes'[cIdx] && c < cubes'[cIdx + 1])
                         |> Array.length)

                result <-
                    Array.append
                        [| for r in 1..newRocks do
                               (row, base' + r) |]
                        result

            result)
    | South ->
        let grouped = rocks |> Array.groupBy snd

        grouped
        |> Array.map (fun (col, coords) ->
            let cubes' =
                cubes
                |> Array.filter (fun c -> c |> snd = col)
                |> Array.map fst
                |> Array.sort
                |> Array.rev

            let mutable result = [||]

            for cIdx in -1 .. (cubes'.Length - 1) do
                let base', newRocks =
                    if (cubes' |> Array.isEmpty) then
                        dim, coords |> Array.length
                    elif cIdx = -1 then
                        dim, (coords |> Array.filter (fun (r, _) -> r > cubes'[0]) |> Array.length)
                    elif cIdx = cubes'.Length - 1 then
                        cubes'[cIdx], (coords |> Array.filter (fun (r, _) -> r < cubes'[cIdx]) |> Array.length)
                    else
                        cubes'[cIdx],
                        (coords
                         |> Array.filter (fun (r, _) -> r < cubes'[cIdx] && r > cubes'[cIdx + 1])
                         |> Array.length)

                result <-
                    Array.append
                        [| for r in 1..newRocks do
                               (base' - r, col) |]
                        result

            result)
    | East ->
        let grouped = rocks |> Array.groupBy fst

        grouped
        |> Array.map (fun (row, coords) ->
            let cubes' =
                cubes
                |> Array.filter (fun c -> c |> fst = row)
                |> Array.map snd
                |> Array.sort
                |> Array.rev

            let mutable result = [||]

            for cIdx in -1 .. (cubes'.Length - 1) do
                let base', newRocks =
                    if (cubes' |> Array.isEmpty) then
                        dim, coords |> Array.length
                    elif cIdx = -1 then
                        dim, (coords |> Array.filter (fun (_, c) -> c > cubes'[0]) |> Array.length)
                    elif cIdx = cubes'.Length - 1 then
                        cubes'[cIdx], (coords |> Array.filter (fun (_, c) -> c < cubes'[cIdx]) |> Array.length)
                    else
                        cubes'[cIdx],
                        (coords
                         |> Array.filter (fun (_, c) -> c < cubes'[cIdx] && c > cubes'[cIdx + 1])
                         |> Array.length)

                result <-
                    Array.append
                        [| for r in 1..newRocks do
                               (row, base' - r) |]
                        result

            result)
    |> Array.concat

let doCycle =
     roll North cubes
     >> roll West cubes
     >> roll South cubes
     >> roll East cubes

let detectCycleBrent (start: (int * int) array) =
    let mutable power = 1L
    let mutable lambda = 1L
    let mutable hareInstr = 0
    let mutable tortoise = start
        
    let mutable hare = start
    hare <- doCycle hare
        
    while tortoise <> hare do
        if power = lambda then
            tortoise <- hare
            power <- power * 2L
            lambda <- 0
        hare <- doCycle hare
        lambda <- lambda + 1L
        
    tortoise <- start
    hare <- start
    hareInstr <- 0
    for _ in 0L .. lambda - 1L do
        hare <- doCycle hare
    
    let mutable mu = 0L
    while tortoise <> hare do
        tortoise <- doCycle tortoise
        hare <- doCycle hare
        mu <- mu + 1L

    lambda, mu
    
let calculateLoad (rocks: (int * int) array) =
    rocks |> Array.map (fun (r, _) -> dim - r) |> Array.sum
    
let lambda, mu = detectCycleBrent rocks
let cyclesToRun = mu + (1000000000L - (mu |> int64)) % lambda

printfn "%A" cyclesToRun

let mutable resultRocks = rocks |> Array.copy

for i in 1L .. cyclesToRun do
    resultRocks <- doCycle resultRocks
    
printfn "%A" (calculateLoad resultRocks)
    
