open System
open System.IO

let readFile input = File.ReadAllLines input

let map = "./inputs/11.txt" |> readFile |> Array.map (fun s -> s.ToCharArray())

let map2d = array2D map

let mutable emptyRows = Set.empty

for row in 0 .. (map2d |> Array2D.length1) - 1 do
    if map2d[row, *] |> Array.forall (fun c -> c = '.') then
        emptyRows <- emptyRows.Add row

let mutable emptyCols = Set.empty

for col in 0 .. (map2d |> Array2D.length2) - 1 do
    if map2d[*, col] |> Array.forall (fun c -> c = '.') then
        emptyCols <- emptyCols.Add col

let rec makePairs (input: (int * int) list) (output: ((int * int) * (int * int)) list) =
    match input with
    | [] -> output
    | h :: t -> makePairs t (List.append output (t |> List.map (fun p -> h, p)))

let listOfPoints =
    map
    |> Array.mapi (fun r row ->
        row
        |> Array.mapi (fun c col -> ((r, c), col))
        |> Array.filter (fun (_, col) -> col = '#'))
    |> Array.reduce Array.append
    |> Array.map fst
    |> List.ofArray
    
let pairs = makePairs listOfPoints []

pairs
|> List.map (fun ((ar, ac), (br, bc)) ->
                let rMin, rMax = if br - ar < 0 then br, ra else ar, br
                let cMin, cMax = if bc - ac < 0 then bc, ac else ac, bc
                
                    )

printfn "%A" pairs
printfn "%A %A" emptyRows emptyCols
