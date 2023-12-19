open System
open System.IO

let parseHexInstructions (hex: string) =
    let dir = match hex[7] with
                | '0' -> "R"
                | '1' -> "D"
                | '2' -> "L"
                | '3' -> "U"
    let dist = hex.Substring(2, 5) |> (fun h -> Convert.ToInt32(h, 16))
    dir, dist

let instructions =
    "./inputs/18.txt"
    |> File.ReadAllLines
    |> Array.map (fun l -> l.Split(" ") |> (fun l' -> l'[0], (l'[1] |> int), parseHexInstructions l'[2]))

let justDirs = instructions |> Array.map (fun (_, _, (d, n)) -> (d, n))

let mutable verts: (float * float) list = [ 0, 0 ]
let mutable curr = 0, 0
let mutable prevDir = justDirs |> Array.last |> fst

for (d, x) in justDirs do
    match d with
    | "R" -> curr <- fst curr, snd curr + x
    | "D" -> curr <- fst curr + x, snd curr
    | "U" -> curr <- fst curr - x, snd curr
    | "L" -> curr <- fst curr, snd curr - x
    | _ -> raise (Exception "shouldn't be here")

    let (prevR, prevC) :: otherVerts = verts

    match prevDir, d with
    | "D", "R"
    | "R", "D" -> verts <- (prevR - 0.5, prevC + 0.5) :: otherVerts
    | "D", "L"
    | "L", "D" -> verts <- (prevR + 0.5, prevC + 0.5) :: otherVerts
    | "U", "L"
    | "L", "U" -> verts <- (prevR + 0.5, prevC - 0.5) :: otherVerts
    | "U", "R"
    | "R", "U" -> verts <- (prevR - 0.5, prevC - 0.5) :: otherVerts

    verts <- (curr |> fst |> float, curr |> snd |> float) :: verts
    prevDir <- d

let mutable intVerts = verts |> List.map (fun (r, c) -> (r + 0.5) |> int64, (c + 0.5) |> int64) |> List.tail
intVerts <- (intVerts |> List.last) :: intVerts

let rec area2 (verts: (int64 * int64) list) =
    match verts with
    | [] -> 0L
    | [ _ ] -> 0L
    | (r0, c0) :: (r1, c1) :: others ->
        let current = r0 * c1 - r1 * c0
        current + (area2 ((r1, c1) :: others))

let area = area2 >> (fun f -> f / 2L)

let rec perimeter (verts: (int * int) list) =
    match verts with
    | [] -> 0
    | [ _ ] -> 0
    | (r0, c0) :: (r1, c1) :: others -> abs ((r1 - r0) + (c1 - c0)) + (perimeter ((r1, c1) :: others))

let internalArea totalArea points = totalArea - (points / 2) + 1

printfn "%A" intVerts
printfn "%A" (area intVerts)
// printfn "%A" (perimeter verts)
// printfn "%A" (internalArea (area verts) (perimeter verts))
