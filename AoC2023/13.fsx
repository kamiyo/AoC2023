open System
open System.IO

let rec rotateStringArray (map: string array) (length: int) (depth: int) =
    match depth with
    | x when x = length -> []
    | x ->
        (map |> Array.map (fun s -> s.Substring(x, 1)) |> (fun sa -> String.Join("", sa)))
        :: (rotateStringArray map length (depth + 1))

let findAxis (map: string array) =
    let main = map[.. (map.Length - 2)] |> Array.indexed
    let shifted = map[1..]

    let axis =
        Array.fold2 (fun state (idx, main) shifted -> if main = shifted then idx :: state else state) [] main shifted
    // Check all other ones
    axis
    |> List.filter (fun a ->
        let mutable original = map[..a]
        let mutable reflected = map[(a + 1) ..]
        let lengthDiff = reflected.Length - original.Length

        if lengthDiff > 0 then
            reflected <- reflected |> Array.take original.Length
        else
            original <- original |> Array.skip -lengthDiff

        reflected |> Array.Reverse
        // printfn "%A %A" original reflected
        Array.forall2 (fun l r -> l = r) original reflected)
    |> List.map (fun a -> a + 1)


type Axis =
    | Horizontal of int
    | Vertical of int

let maps =
    "./inputs/13.txt"
    |> File.ReadAllLines
    |> (fun f -> String.Join("\n", f).Split("\n\n") |> Array.map (fun m -> m.Split("\n")))
    |> Array.map (fun m ->
        match findAxis m with
        | [] ->
            rotateStringArray m (m[0].Length) 0
            |> Array.ofList
            |> findAxis
            |> List.head
            |> Vertical
        | a -> Horizontal(a.Head))
    |> Array.fold
        (fun state mirror ->
            match mirror with
            | Vertical(v) -> state + v
            | Horizontal(h) -> state + (100 * h))
        0
    |> printfn "%A"
