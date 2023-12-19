open System
open System.Collections.Generic
open System.IO


type Direction =
    | North
    | West
    | South
    | East
    | Origin

type MapKey = (int * int) * (Direction * int)

let getNeighborsCoords (((row, col), (dir, dist)): MapKey) =
    let coords = [ (row - 1, col); (row, col - 1); (row + 1, col); (row, col + 1) ]

    let dirDists =
        match dir with
        | North -> [ (North, dist + 1); (West, 1); (South, 1); (East, 1) ]
        | West -> [ (North, 1); (West, dist + 1); (South, 1); (East, 1) ]
        | South -> [ (North, 1); (West, 1); (South, dist + 1); (East, 1) ]
        | East -> [ (North, 1); (West, 1); (South, 1); (East, dist + 1) ]
        | Origin -> [ (North, 2); (West, 2); (South, 2); (East, 2) ]

    List.zip coords dirDists

// This will filter out going straight more than 3 squares
let getNeighbors (map: int array2d) (current: MapKey) =
    getNeighborsCoords current
    |> List.filter (fun ((r, c), _) -> not (r = 0 && c = 0))
    |> List.filter (fun (_, (dir, _)) ->
        match current |> snd |> fst with
        | North -> dir <> South
        | South -> dir <> North
        | West -> dir <> East
        | East -> dir <> West
        | Origin -> false)
    |> List.filter (fun ((r, c), _) -> r >= 0 && c >= 0 && r < (map |> Array2D.length1) && c < (map |> Array2D.length2))
    |> List.filter (fun (_, (dir, dist) as input) ->
        if (current |> snd |> snd) < 4 then
            dir = (current |> snd |> fst)
        else
            true)
    |> List.filter (fun (_, (_, dist)) -> not (dist > 10))


let Dijkstra (map: int array2d) (((r0, c0), _) as start: MapKey) =
    let mutable costSoFar: Map<MapKey, int> = Map([])
    let mutable cameFrom: Map<MapKey, MapKey> = Map([])
    let frontier = PriorityQueue<MapKey, int>()
    costSoFar <- costSoFar |> Map.add start 0
    frontier.Enqueue(start, 0)
    costSoFar <- costSoFar |> Map.add start map[r0, c0]
    cameFrom <- cameFrom.Add(start, ((0, 0), (Origin, 0)))

    let mutable break = false
    while frontier.Count <> 0 && not break do
        let current = frontier.Dequeue()
        
        if (current |> fst) = ((map |> Array2D.length1) - 1, (map |> Array2D.length2) - 1) && (current |> snd |> snd) >= 4 then
            printfn "%A" costSoFar[current]
            break <- true
            

        let neighbors = getNeighbors map current
        // printfn "%A: %A" current neighbors

        for ((nr, nc), _) as next in neighbors do
            let newCost = costSoFar[current] + map[nr, nc]

            if (costSoFar.ContainsKey(next) |> not) || newCost < costSoFar[next] then
                costSoFar <- costSoFar.Add(next, newCost)
                frontier.Enqueue(next, newCost)
                cameFrom <- cameFrom.Add(next, current)

    cameFrom

let rec buildPath (paths: Map<MapKey, MapKey>) (curr: MapKey) =
    if (fst curr) = (0, 0) then
        [ curr ]
    else
        curr :: (buildPath paths paths[curr])

let map =
    "./inputs/17.txt"
    |> File.ReadAllLines
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> Int32.Parse(c.ToString())))
    |> array2D

let paths =
    [| Dijkstra map ((0, 1), (East, 1)); Dijkstra map ((1, 0), (South, 1)) |]
    |> Array.Parallel.map (fun m ->
        let ends =
            m
            |> Map.filter (fun ((r, c), _) _ -> r = (Array2D.length1 map) - 1 && c = (Array2D.length2 map) - 1)
            |> Map.keys
            |> List.ofSeq

        ends |> List.map (buildPath m))
    |> Array.reduce List.append

// printfn "%A" paths

let heatLoss =
    paths
    |> List.map (fun p -> p |> List.map (fun ((r, c), _) -> map[r, c]) |> List.sum)



printfn "%A" ((heatLoss |> List.min) - map[0, 0])
// printfn "%A" heatLoss
