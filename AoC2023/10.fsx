open System
open System.IO

let readFile input = File.ReadAllLines input

type Coord = int * int

type Direction =
    | North
    | East
    | West
    | South

let turn (dir: Direction) (turn: char) =
    match dir, turn with
    | North, 'F' -> East
    | North, '7' -> West
    | East, '7' -> South
    | East, 'J' -> North
    | South, 'J' -> West
    | South, 'L' -> East
    | West, 'L' -> North
    | West, 'F' -> South
    | North, '|' -> North
    | South, '|' -> South
    | East, '-' -> East
    | West, '-' -> West
    | _ -> raise (Exception "Shouldn't be here")

let findInitialDirs (surrounding: (Direction * char) list) =
    surrounding
    |> List.filter (fun (dir, path) ->
        match dir with
        | North when [ 'F'; '7'; '|' ] |> List.contains path -> true
        | East when [ '7'; 'J'; '-' ] |> List.contains path -> true
        | West when [ 'L'; 'F'; '-' ] |> List.contains path -> true
        | South when [ 'J'; 'L'; '|' ] |> List.contains path -> true
        | _ -> false)

let goForwards (coord: Coord) (dir: Direction) =
    match dir with
    | North -> Coord(fst coord - 1, snd coord)
    | East -> Coord(fst coord, snd coord + 1)
    | West -> Coord(fst coord, snd coord - 1)
    | South -> Coord(fst coord + 1, snd coord)

let rec goAroundPathBidir
    (start: Coord)
    (mapping: string array)
    ((aCoord, aDir): Coord * Direction)
    ((bCoord, bDir): Coord * Direction)
    // (dist: int)
    (travelled: Set<int * char> array)
    =
    if aCoord = bCoord && aCoord <> start then
        travelled
    else
        let newA = goForwards aCoord aDir
        let newB = goForwards bCoord bDir
        let aChar = mapping[fst newA].Chars(snd newA)
        let bChar = mapping[fst newB].Chars(snd newB)
        let newAdir = turn aDir aChar
        let newBdir = turn bDir bChar
        travelled[fst newA] <- travelled[fst newA] |> Set.add (snd newA, aChar)
        travelled[fst newB] <- travelled[fst newB] |> Set.add (snd newB, bChar)
        goAroundPathBidir start mapping (newA, newAdir) (newB, newBdir) travelled

let mapping = "./inputs/10.txt" |> readFile

let start =
    mapping
    |> Array.mapi (fun idx row ->
        match row.IndexOf 'S' with
        | -1 -> None
        | a -> Some(idx, a))
    |> Array.filter Option.isSome
    |> Array.head
    |> Option.get

let a :: b :: _ =
    [ (fst start - 1, snd start, North)
      (fst start, snd start + 1, East)
      (fst start, snd start - 1, West)
      (fst start + 1, snd start, South) ]
    |> List.filter (fun (r, c, _) ->
        let result =
            try
                mapping[r].Chars c
            with :? IndexOutOfRangeException ->
                '~'

        result <> '~')
    |> List.map (fun (r, c, d) -> d, mapping[r].Chars c)
    |> findInitialDirs

let initialChar =
    match (fst a), (fst b) with
    | North, South
    | South, North -> '|'
    | East, West
    | West, East -> '-'
    | North, East
    | East, North -> 'L'
    | North, West
    | West, North -> 'J'
    | South, West
    | West, South -> '7'
    | South, East
    | East, South -> 'F'
    | _ -> raise (Exception "invalid directions")

let accumulator: Set<int * char> array =
    (Set.empty |> Array.replicate (mapping |> Array.length))

accumulator[fst start] <- Set.singleton (snd start, initialChar)
let path = goAroundPathBidir start mapping (start, fst a) (start, fst b) accumulator

// printfn "%A" path
let mutable pointsIn = 0

for (idy, row) in mapping |> Array.indexed do
    for (idx, col) in row.ToCharArray() |> Array.indexed do
        let rowPath = path[idy]

        if rowPath |> Set.contains (idx, col) || col = 'S' then
            ()
        else
            let numToTheRight =
                rowPath
                |> Set.filter (fun input ->
                    match input with
                    | x, '|'
                    | x, 'J'
                    | x, 'L' when x > idx -> true
                    | _ -> false)
                |> Set.count


            pointsIn <- pointsIn + (numToTheRight % 2)

printfn "%A" pointsIn
