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
    |> List.ofArray

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

let addDefaults (startSeed: uint) (endSeed: uint) (dests: uint array list) =
    let mutable sorted = dests |> List.sortBy (fun d -> d[0])
    let mutable toAdd: uint array list = []
    let mutable current = sorted
    let mutable previous = startSeed

    while previous < endSeed do
        match current with
        | [] ->
            toAdd <- [| previous; endSeed - previous |]::toAdd
            previous <- endSeed
        | head::tail ->
            current <- tail
            match head[0] with
            | m when m > previous ->
                toAdd <- [| previous; m - previous |]::toAdd
            | _ -> 
                ()
            previous <- head[0] + head[1]
    
    List.append dests toAdd |> List.sortBy (fun d -> d[0])
    
let getDest (map: uint array list) (sources: uint array list) =
    let sorted = sources |> List.sortBy (fun s -> s[0])
    let mutable currentSources = sorted
    let mutable (destinations: uint array list) = []

    while currentSources |> List.isEmpty |> not do
        printfn "%A" currentSources

        let source :: tail = currentSources
        currentSources <- tail
        
        map
        |> List.iter (fun mapping ->
            printfn "%A %A" mapping source
            match overlaps (Array.sub mapping 1 2) source with
            | None -> ()
            | Some(result) ->
                let destRange = [| result.Overlap[0] + (mapping[1] - mapping[0]); result.Overlap[1] |]
                destinations <- destRange::destinations
                printfn "%A" result                    
                currentSources <- List.append currentSources result.Others                
                ())
        
        
        
    destinations |> addDefaults (sorted |> List.head |> Array.item 0) (sorted |> List.last |> Array.reduce (fun l r -> l + r))
        


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
            |> getDest humLoc)

    printfn "%A" locations

"./inputs/05.txt" |> openFile |> getClosestLocation |> printfn "%A"
