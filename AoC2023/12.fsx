open System
open System.IO

let rec combinations accum size set =
    seq {
        match size, set with
        | n, x :: xs ->
            if n > 0 then
                yield! combinations (x :: accum) (n - 1) xs

            if n >= 0 then
                yield! combinations accum n xs
        | 0, [] -> yield accum
        | _, [] -> ()
    }

let dict = new System.Collections.Generic.Dictionary<int * int, uint64>()

let rec searchPossibilities (springs: char array) (damaged: int list) (index: int) =
    // printfn "%A %A" springs damaged
    let mutable newIndex = index
    // printfn "%A %A %A" index springs damaged

    let result =
        match damaged with
        | [] ->
            if index < (springs |> Array.length) then
                match springs[index..] with
                | [||] ->
                    // printfn "Success"
                    1UL
                | remaining when remaining |> Array.contains '#' |> not ->
                    // printfn "Success"
                    1UL
                | _ ->
                    // printfn "Not found"
                    0UL
            else
                // printfn "Success"
                1UL
        | currDamaged :: restDamaged ->
            
            newIndex <- try Array.FindIndex(springs, index, (fun s -> s = '?' || s = '#')) with
                        | :? ArgumentOutOfRangeException -> -1
            // printfn "newIndex %A %A %A" newIndex springs[newIndex..] damaged

            if newIndex = -1 then
                0UL
            else
                let mutable v = 99UL

                match dict.TryGetValue((newIndex, damaged |> List.length), &v) with
                | true -> v
                | false ->
                    let mustCheck =
                        let recordLength = springs |> Array.length

                        if (newIndex + currDamaged - 1) >= recordLength then
                            0UL
                        elif
                            springs[newIndex .. (newIndex + currDamaged - 1)]
                            |> Array.forall (fun s -> s = '?' || s = '#')
                            && (newIndex + currDamaged = (springs |> Array.length)
                                || springs[newIndex + currDamaged] <> '#')
                        then
                            searchPossibilities springs restDamaged (newIndex + currDamaged + 1)
                        else
                            0UL

                    // printfn "mustCheck %A %A" mustCheck newIndex

                    mustCheck
                    + (if springs[newIndex] = '?' then
                           searchPossibilities springs damaged (newIndex + 1)
                       else
                           0UL)

    dict.TryAdd((newIndex, damaged |> List.length), result) |> ignore
    result


// | [], a::x -> 0UL
// | '#'::restSprings, currDamaged::restDamaged ->
//     if springs |> List.length < currDamaged then
//         0UL
//     elif springs |> List.take currDamaged |> List.forall (fun s -> s = '?' || s = '#') |> not then
//         0UL
//     elif springs |> List.length = currDamaged then
//         if restDamaged = [] then
//             1UL
//         else
//             0UL
//     else
//         let jumped = springs |> List.skip currDamaged |> List.head
//         if jumped = '.' || jumped = '?' then
//             searchPossibilities (springs |> List.skip (currDamaged + 1)) restDamaged
//         else
//             0UL
// | '.'::restSprings, _ -> searchPossibilities restSprings damaged
// | '?'::restSprings, currDamaged::restDamaged ->
//     let pound =
//         if springs |> List.length < currDamaged then
//             0UL
//         elif springs |> List.take currDamaged |> List.forall (fun s -> s = '?' || s = '#') |> not then
//             0UL
//         elif springs |> List.length = currDamaged then
//             if restDamaged = [] then
//                 1UL
//             else
//                 0UL
//         else
//             let jumped = springs |> List.skip currDamaged |> List.head
//             if jumped = '.' || jumped = '?' then
//                 searchPossibilities (springs |> List.skip (currDamaged + 1)) restDamaged
//             else
//                 0UL
//     pound + searchPossibilities restSprings damaged

let counter = ref -1

let mapping =
    "./inputs/12.txt"
    |> File.ReadAllLines
    |> Array.mapi (fun idx line ->
        dict.Clear()
        let parts = line.Split " "
        // printfn "%A" idx


        let damaged =
            parts[1].Split "," |> Array.map int |> List.ofArray
            |> List.replicate 5
            |> List.concat

        let row =
            parts[0]
            |> Array.replicate 5
            |> (fun s -> String.Join('?', s).ToCharArray())
        // |> (fun s -> s.ToCharArray())
        // |> List.ofArray

        searchPossibilities row damaged 0)
    |> Array.reduce (fun l r -> l + r)
    |> printfn "%A"
