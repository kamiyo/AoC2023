open System
open System.IO

let openFile input = File.ReadAllLines input

let parseInput (input: string) =
    let split = input.Replace(" ", "").Split(':')
    split |> Array.last |> uint64
    
let calculateDistances ((time, distance): uint64 * uint64) =
    let mutable timeList: uint64 list = []
    for hold in 0UL .. time do
        let remaining = time - hold
        let traveled = remaining * hold
        if traveled > distance then
            timeList <- traveled::timeList
        else
            ()
    
    timeList
    
"./inputs/06.txt"
|> openFile
|> Array.map parseInput
|> (fun arrays -> arrays[0], arrays[1])
|> calculateDistances
|> List.length
|> printfn "%A"