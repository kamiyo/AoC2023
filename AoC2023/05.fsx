open System
open System.IO
open System.Text.RegularExpressions

let openFile file = File.ReadAllLines file

let getSeeds sections =
    Array.find
        (fun (elm: string array) ->
            let regexp = new Regex(@"seeds")
            regexp.IsMatch(elm[0]))
        sections
    |> Array.item 0
    |> Regex(@"seeds: (.*)").Match
    |> (fun m -> m.Groups[1].Value)
    |> (fun s -> s.Split(" "))
    |> Array.map uint64
    |> Array.chunkBySize 2
    |> Array.map (fun s -> Array.append s [| s[0] |])

let getMap which sections =
    Array.find
        (fun (elm: string array) ->
            let regexp = new Regex(which)
            regexp.IsMatch(elm[0]))
        sections
    |> Array.tail
    |> Array.map (fun l -> l.Split(" ") |> Array.map uint64)
    |> List.ofArray

type OverlapResult =
    { Overlap: uint64 array
      Others: uint64 array list }

let overlaps (haystack: uint64 array) (needle: uint64 array) =
    let hl = haystack[0]
    let hr = haystack[0] + haystack[1]
    let nl = needle[0]
    let nr = needle[0] + needle[1]

    if (nl >= hl && nr <= hr) then
        { Overlap = needle; Others = [] } |> Some
    elif (nl < hl && nr > hr) then
        { Overlap = haystack
          Others = [ [| nl; hl - nl |]; [| hr; nr - hr |] ] }
        |> Some
    elif (nl < hl && nr > hl) then
        { Overlap = [| hl; nr - hl |]
          Others = [ [| nl; hl - nl |] ] }
        |> Some
    elif (nr >= hr && nl < hr) then
        { Overlap = [| nl; hr - nl |]
          Others = [ [| hr; nr - hr |] ] }
        |> Some
    else
        None


let getDest (map: uint64 array list) (sources: uint64 array list) =
    let sorted = sources |> List.sortBy (fun s -> s[0])
    let mutable currentSources = sorted
    let mutable (destinations: uint64 array list) = []
    // printfn "[][][][][]"

    while currentSources |> List.isEmpty |> not do
        // printfn "sources: %A" currentSources

        let source :: tail = currentSources
        currentSources <- tail
        let mutable alreadyPushed = false

        map
        |> List.iteri (fun index mapping ->
            // printfn "--- iter start"
            // printfn "mapping: %A, source: %A" mapping source
            // printfn "dests: %A" destinations
            // printfn "alreadyPushed: %A" alreadyPushed

            match overlaps (Array.sub mapping 1 2) source, alreadyPushed with
            | None, false when index = (map.Length - 1) -> destinations <- source :: destinations
            | Some(result), false ->
                // printfn "overlap result: %A" result

                let destRange =
                    [| result.Overlap[0] + (mapping[0] - mapping[1])
                       result.Overlap[1]
                       source[2] + result.Overlap[0] - source[0] |]

                // printfn "new dest: %A" destRange
                destinations <- destRange :: destinations

                let others =
                    result.Others
                    |> List.map (fun o -> Array.append o [| source[2] + o[0] - source[0] |])

                currentSources <- List.append currentSources others
                alreadyPushed <- true
                ()
            | _, _ -> ())

    destinations



let getClosestLocation (input: string array) =
    let sections =
        String.Join("\n", input).Split("\n\n") |> Array.map (fun l -> l.Split("\n"))

    let seeds = getSeeds sections
    let seedSoil = getMap @"seed-to-soil" sections
    let soilFert = getMap @"soil-to-fertilizer" sections
    let fertWater = getMap @"fertilizer-to-water" sections
    let waterLight = getMap @"water-to-light" sections
    let lightTemp = getMap @"light-to-temperature" sections
    let tempHum = getMap @"temperature-to-humidity" sections
    let humLoc = getMap @"humidity-to-location" sections

    let locations =
        seeds
        |> List.ofArray
        |> List.map (fun seed ->
            [ seed ]
            |> getDest seedSoil
            |> getDest soilFert
            |> getDest fertWater
            |> getDest waterLight
            |> getDest lightTemp
            |> getDest tempHum
            |> getDest humLoc
            )
        |> List.reduce List.append
        |> List.sortBy (fun m -> m[0])

    locations

"./inputs/05.txt" |> openFile |> getClosestLocation |> List.minBy (fun m -> m[0]) |> printfn "%A"
