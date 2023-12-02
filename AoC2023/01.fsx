open System
open System.IO

let openFile filename = File.ReadAllLines filename

let spelled =
    [| "zero"
       "one"
       "two"
       "three"
       "four"
       "five"
       "six"
       "seven"
       "eight"
       "nine" |]

let getDigits () =
    openFile "inputs/01.txt"
    |> Array.map (fun line ->
        let filtered = line |> String.filter Char.IsDigit
        let first = Seq.head filtered
        let last = Seq.last filtered
        let together = first.ToString() + last.ToString()
        printfn $"{together}"
        int together)
    |> Array.reduce (fun l r -> l + r)

let getDigitsSpelled () =
    openFile "inputs/01.txt"
    |> Array.map (fun line ->
        let firstDigit = Seq.tryFindIndex Char.IsDigit line
        let lastDigit = Seq.tryFindIndexBack Char.IsDigit line
        let positions = spelled |> Array.map line.IndexOf
        let backPos = spelled |> Array.map line.LastIndexOf

        let firstSpelledIndex =
            positions
            |> Array.reduce (fun l r ->
                match (l, r) with
                | (-1, _) -> r
                | (_, -1) -> l
                | _ -> min l r)

        let firstSpelled = Array.IndexOf(positions, firstSpelledIndex)
        let lastSpelledIndex = backPos |> Array.reduce max
        let lastSpelled = Array.IndexOf(backPos, lastSpelledIndex)
        let first = match (firstDigit, firstSpelledIndex) with
                    | (Some(d), -1) -> Char.GetNumericValue line[d] |> int
                    | (Some(d), n) when d < n-> Char.GetNumericValue line[d] |> int
                    | _ -> firstSpelled
        let last = match (lastDigit, lastSpelledIndex) with
                    | (Some(d), -1) -> Char.GetNumericValue line[d] |> int
                    | (Some(d), n) when d > n -> Char.GetNumericValue line[d] |> int
                    | _ -> lastSpelled
        printfn $"{firstSpelledIndex} {lastSpelledIndex} {firstDigit} {lastDigit} -> {first}{last}"
        first.ToString() + last.ToString() |> int)
    |> Array.reduce (fun l r -> l + r)

printfn $"{{%d{getDigitsSpelled ()}}}"
