open System
open System.IO
open System.Text.RegularExpressions

let openFile filename = File.ReadAllLines filename

let matches = 
    "./inputs/04.txt"
    |> openFile
    |> Array.map (fun line ->
        let numbers = line.Split(": ")[1]
        let split = numbers.Split(" | ")

        let win =
            split[0].Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int

        let mine =
            split[1].Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map int

        let filtered = win |> Array.filter (fun w -> Array.contains w mine)

        filtered.Length)

let mutable initial = Array.replicate matches.Length 1

for j = 0 to matches.Length - 1 do
    let numberWins = matches[j]
    printfn "%A %A" j numberWins
    for i = 1 to numberWins do
        initial[j + i] <- initial[j + i] + initial[j]
        
printfn "%A" (initial |> Array.reduce (fun l r -> l + r))
