open System
open System.IO
open System.Text.RegularExpressions

let openFile filename = File.ReadAllLines filename

let symbols = [| '@'; '*'; '/'; '='; '&'; '#'; '+'; '-'; '$'; '%' |]

let regex = new Regex(@"(\d+)", RegexOptions())

let parseLine totalRows idx (line: string) =
    let lineLength = line.Length
    let result = regex.Matches(line)
    let indices = result |> Seq.map (fun m -> m.Index) |> List.ofSeq
    let lengths = result |> Seq.map (fun m -> m.Length) |> List.ofSeq

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

    result |> List.ofSeq |> List.map (fun m -> m.Groups[1].Value |> int), (toCheck |> List.ofSeq)
// printfn "%A %A %A %A" result indices lengths (toCheck |> List.ofSeq)

let rec checkSurrounding (grid: string array) (cells: (int * int) list) (accum: (int * int) list) =
    match cells with
    | (x, y) :: t ->
        match grid[y].ToCharArray()[x] with
        | '*' -> checkSurrounding grid t ((x, y) :: accum)
        | _ -> checkSurrounding grid t accum
    | [] -> accum

let input = "./inputs/03.txt" |> openFile
let rows = input.Length

input
|> Array.mapi (parseLine rows)
|> Array.map (fun (nums, coords) -> List.map2 (fun n c -> checkSurrounding input c [], n) nums coords)
|> Array.reduce List.append
|> List.fold (fun state (cs, n) -> cs |> List.map (fun c -> c, n) |> List.append state) ([]: ((int * int) * int) list)
|> List.fold
    (fun (map: Map<int * int, int list>) (c, n) ->
        match map.TryFind c with
        | None -> map.Add(c, [ n ])
        | Some(l) -> map.Add(c, n :: l))
    Map.empty
// |> List.filter (fun l -> l |> fst |> List.isEmpty |> not)
// |> List.map Option.get
// |> List.reduce (fun l r -> l + r)
|> Map.toList
|> List.filter (fun (_, ns) -> ns.Length = 2)
|> List.map (fun (_, ns) -> ns |> List.reduce (fun l r -> l * r))
|> List.reduce (fun l r -> l + r)
|> printfn "%A"
