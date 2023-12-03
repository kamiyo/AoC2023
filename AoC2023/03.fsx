open System
open System.IO
open System.Text.RegularExpressions

let openFile filename = File.ReadAllLines filename

let symbols = [| "@"; "*"; "/"; "="; "&"; "#"; "+"; "-"; "$"; "%" |]

let regex = new Regex(@"(\d+)", RegexOptions())

let parseLine totalRows idx (line: string) =
    let lineLength = line.Length
    let result = regex.Matches(line)
    let indices = result |> Seq.map (fun m -> m.Index)
    let lengths = result |> Seq.map (fun m -> m.Length)

    let toCheck =
        Seq.map2
            (fun pos length ->
                [ for y in [ (idx - 1) .. (idx + 1) ] do
                      for x in (pos - 1) .. (pos + length) do
                          if y = idx && x >= pos && x < pos + length then
                              ()
                          else
                              yield (x, y) ]
                |> Seq.filter (fun coord ->
                    (fst coord) >= 0
                    && (fst coord) < lineLength
                    && (snd coord) >= 0
                    && (snd coord) < totalRows)
                |> List.ofSeq)
            indices
            lengths
        
        
    printfn "%A %A %A %A" result indices lengths (toCheck |> List.ofSeq)

let input = "./inputs/03.txt" |> openFile
let rows = input.Length
input |> Array.iteri (parseLine rows)
