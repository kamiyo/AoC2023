open System
open System.IO

let map =
    "./inputs/16.txt"
    |> File.ReadAllLines
    |> Array.map (fun line -> line.ToCharArray())
    |> array2D

// This will house a bitmask: 1 = North, 2 = West, 4 = South, 8 = East

[<Flags>]
type Directions =
    | North = 0b0001
    | West = 0b0010
    | South = 0b0100
    | East = 0b1000

let makeMap () = Array2D.zeroCreate (map |> Array2D.length1) (map |> Array2D.length2)
let mutable visited: Directions array2d = makeMap()
    


let rec traverse (mapping: char array2d) ((row, col): (int * int)) (dir: Directions) =
    if
        (row < 0
         || col < 0
         || row >= (mapping |> Array2D.length1)
         || col >= (mapping |> Array2D.length2))
    then
        ()
    elif visited[row, col].HasFlag(dir) then
        ()
    else
        visited[row, col] <- visited[row, col] ||| dir

        match dir with
        | Directions.North ->
            match mapping[row, col] with
            | '|'
            | '.' -> traverse mapping (row - 1, col) dir
            | '-' ->
                traverse mapping (row, col + 1) Directions.East
                traverse mapping (row, col - 1) Directions.West
            | '/' -> traverse mapping (row, col + 1) Directions.East
            | '\\' -> traverse mapping (row, col - 1) Directions.West
            | _ -> raise (Exception "Should not be here")
        | Directions.South ->
            match mapping[row, col] with
            | '|'
            | '.' -> traverse mapping (row + 1, col) dir
            | '-' ->
                traverse mapping (row, col + 1) Directions.East
                traverse mapping (row, col - 1) Directions.West
            | '/' -> traverse mapping (row, col - 1) Directions.West
            | '\\' -> traverse mapping (row, col + 1) Directions.East
            | _ -> raise (Exception "Should not be here")
        | Directions.West ->
            match mapping[row, col] with
            | '-'
            | '.' -> traverse mapping (row, col - 1) dir
            | '|' ->
                traverse mapping (row - 1, col) Directions.North
                traverse mapping (row + 1, col) Directions.South
            | '/' -> traverse mapping (row + 1, col) Directions.South
            | '\\' -> traverse mapping (row - 1, col) Directions.North
            | _ -> raise (Exception "Should not be here")
        | Directions.East ->
            match mapping[row, col] with
            | '-'
            | '.' -> traverse mapping (row, col + 1) dir
            | '|' ->
                traverse mapping (row - 1, col) Directions.North
                traverse mapping (row + 1, col) Directions.South
            | '/' -> traverse mapping (row - 1, col) Directions.North
            | '\\' -> traverse mapping (row + 1, col) Directions.South
            | _ -> raise (Exception "Should not be here")

// printfn "%A" map
// traverse map (0, 0) Directions.East
// printfn "%A" (visited |> Array2D.map (fun v -> v.GetHashCode()) |> Seq.cast<int> |> Seq.filter (fun v -> v <> 0) |> Seq.length)

let getVisitedLength (v: Directions array2d) =
    v |> Seq.cast<int> |> Seq.filter (fun v' -> v'.GetHashCode() <> 0) |> Seq.length

let mutable visitedLengths = Set<int>([])

for col in 0..((map |> Array2D.length2) - 1) do
    visited <- makeMap()
    traverse map (0, col) Directions.South
    visitedLengths <- visitedLengths |> Set.add (getVisitedLength visited)

for col in 0..((map |> Array2D.length2) - 1) do
    visited <- makeMap()
    traverse map ((map |> Array2D.length1) - 1, col) Directions.North
    visitedLengths <- visitedLengths |> Set.add (getVisitedLength visited)

for row in 0..((map |> Array2D.length1) - 1) do
    visited <- makeMap()
    traverse map (row, 0) Directions.East
    visitedLengths <- visitedLengths |> Set.add (getVisitedLength visited)

for row in 0..((map |> Array2D.length1) - 1) do
    visited <- makeMap()
    traverse map (row, (map |> Array2D.length2) - 1) Directions.West
    visitedLengths <- visitedLengths |> Set.add (getVisitedLength visited)
    
printfn "%A" (visitedLengths |> Set.toList |> List.rev)