open System
open System.IO
open System.Text.RegularExpressions

type Game =
    { Red: int
      Green: int
      Blue: int
      Id: int }

let openFile filename = File.ReadAllLines filename

let parseHand (id, hand) =
    let regex =
        new Regex(@"(((?:(?<blue>\d+) blue)|(?:(?<red>\d+) red)|(?:(?<green>\d+) green))(?:, )?)+")

    let result = regex.Match hand

    let blue =
        match result.Groups.Item("blue").Success with
        | false -> 0
        | true -> int (result.Groups.Item("blue").Value)

    let red =
        match result.Groups.Item("red").Success with
        | false -> 0
        | true -> int (result.Groups.Item("red").Value)

    let green =
        match result.Groups.Item("green").Success with
        | false -> 0
        | true -> int (result.Groups.Item("green").Value)

    { Id = id
      Blue = blue
      Green = green
      Red = red }

let parseLine line =
    let regex = new Regex(@"^Game (\d+): (.*)$")
    let result = regex.Match line
    let gameNum = result.Groups[1].Value |> int

    let game =
        result.Groups[2].Value.Split("; ")
        |> Array.map (fun h -> parseHand (gameNum, h))
        |> Array.reduce (fun l r ->
            { Id = gameNum
              Blue = max l.Blue r.Blue
              Red = max l.Red r.Red
              Green = max l.Green r.Green })

    game

let checkGame game =
    game.Red <= 12 && game.Green <= 13 && game.Blue <= 14

let calculatePower game =
    game.Red * game.Green * game.Blue

// "./inputs/02.txt"
// |> openFile
// |> Array.map parseLine
// |> Array.filter checkGame
// |> Array.fold (fun state v -> state + v.Id) 0
// |> (printfn "%d")

"./inputs/02.txt"
|> openFile
|> Array.map parseLine
|> Array.map calculatePower
|> Array.reduce (fun l r -> l + r)
|> (printfn "%d")