open System
open System.IO

let rec rotateStringArray (map: char array array) (length: int) (depth: int) =
    match depth with
    | x when x = length -> []
    | x -> (map |> Array.map (fun s -> s[x])) :: (rotateStringArray map length (depth + 1))

let findAxis (map: char array array) =
    let main = map[.. (map.Length - 2)] |> Array.indexed
    let shifted = map[1..]

    let axis =
        Array.fold2
            (fun state (idx, main') shifted' ->
                if Array.forall2 (fun l r -> l = r) main' shifted' then
                    idx :: state
                elif Array.map2 (fun l r -> l = r) main' shifted' |> Array.filter not |> Array.length = 1 then
                    idx :: state
                else
                    state)
            []
            main
            shifted
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
        Array.map2
            (fun l r ->
                let discrepancies =
                    Array.map2 (fun l' r' -> l' = r') l r |> Array.filter not |> Array.length

                discrepancies)
            original
            reflected
        |> Array.sum
        |> (=) 1)
    |> List.map (fun a -> a + 1)


type Axis =
    | Horizontal of int
    | Vertical of int

let maps =
    "./inputs/13.txt"
    |> File.ReadAllLines
    |> (fun f ->
        String.Join("\n", f).Split("\n\n")
        |> Array.map (fun m -> m.Split("\n") |> Array.map (fun s -> s.ToCharArray())))
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
