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
    |> Array.map uint
    |> Array.chunkBySize 2

let getMap which sections =
    Array.find
        (fun (elm: string array) ->
            let regexp = new Regex(which)
            regexp.IsMatch(elm[0]))
        sections
    |> Array.tail
    |> Array.map (fun l -> l.Split(" ") |> Array.map uint)

type OverlapResult =
    { Overlap: uint array
      Others: uint array list }

let overlaps (haystack: uint array) (needle: uint array) =
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
    elif (nl < hl && nr >= hl) then
        { Overlap = [| hl; nr - hl |]
          Others = [ [| nl; hl - nl |] ] }
        |> Some
    elif (nl < hr && nr >= hr) then
        { Overlap = [| nl; hr - nl |]
          Others = [ [| hr; nr - hr |] ] }
        |> Some
    else
        None



let getDest (map: uint array list) (sources: uint array list) =
    let mutable currentSources = sources
    let mutable (destinations: uint array list) = []

    while currentSources |> List.isEmpty |> not do
        let source :: tail = currentSources
        currentSources <- tail

        map
        |> List.iter (fun mapping ->
            match overlaps (Array.sub mapping 1 2) source with
            | None -> ()
            | Some(result) ->
                destinations <- result.Overlap::destinations
                currentSources <- List.append currentSources result.Others                
                ())
        
        


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
        |> Array.map (fun seed ->
            seed
            |> getDest seedSoil
            |> getDest soilFert
            |> getDest fertWater
            |> getDest waterLight
            |> getDest lightTemp
            |> getDest tempHum
            |> getDest humLoc)

    Array.reduce min locations

"./inputs/05.txt" |> openFile |> getClosestLocation |> printfn "%A"
