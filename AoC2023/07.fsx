open System
open System.IO

let CardValues =
    Map<char, int>(
        [ ('A', 14)
          ('K', 13)
          ('Q', 12)
          ('T', 10)
          ('9', 9)
          ('8', 8)
          ('7', 7)
          ('6', 6)
          ('5', 5)
          ('4', 4)
          ('3', 3)
          ('2', 2)
          ('J', 1) ]
    )

let getRank (cardsCounts: (char * int) list) =
    match cardsCounts with
    | [ (_, _) ] -> 7
    | (_, first) :: (_, second) :: _ ->
        if first = 5 then
            7
        elif first = 4 then
            6
        elif first = 3 then
            if second = 2 then 5 else 4
        elif first = 2 then
            if second = 2 then 3 else 2
        else
            1
    | _ -> 0
    
let getRankWithJokers (cardsCounts: Map<char, int>) =
    match cardsCounts.TryFind 'J' with
    | None ->
        cardsCounts
        |> Map.toList
        |> List.sortByDescending snd
        |> getRank
    | Some(jCount) ->
        let list = cardsCounts.Remove 'J' |> Map.toList |> List.sortByDescending snd
        match jCount with
        | 1 ->
            match list with
            | [ (_, _) ] -> 7 // 1J + 4x -> 5x
            | (_, first)::(_, second)::_ ->
                if first = 3 then   // 1J + 3x + 1y -> 4x + 1y
                    6
                elif first = 2 then 
                    if second = 2 then // 1J + 2x + 2y -> 3x + 2y
                        5
                    else            // 1J + 2x + 1y + 1z -> 3x + 1y + 1z
                        4
                elif first = 1 then // 1J + 1x + 1y + 1z + 1w -> 2x + 1y + 1z + 1w
                    2
                else                // Should not happen
                    0
            | _ -> 0
        | 2 ->
            match list with
            | [ (_, _) ] -> 7
            | (_, first)::_ ->
                if first = 3 then   // 2J + 3x -> 5x
                    7
                elif first = 2 then  // 2J + 2x + 1y -> 4x + 1y
                    6
                elif first = 1 then // 2J + 1x + 1y + 1z -> 3x + 1y + 1z
                    4
                else                // Should not happen
                    0
            | _ -> 0
        | 3 ->
            match list with
            | [ (_, _) ] -> 7
            | (_, first)::_ ->
                if first = 2 then   // 3J + 2x -> 5x
                    7
                elif first = 1 then  // 3J + 1x + 1y -> 4x + 1y
                    6
                else                // Should not happen
                    0
            | _ -> 0
        | 4 ->
            match list with
            | [ (_, _) ] -> 7
            | (_, first)::_ ->
                if first = 1 then   // 4J + 1x -> 5x
                    7
                else                // Should not happen
                    0
            | _ -> 0
        | 5 -> 7
                    
        
let rec compareCardWise (left: string) (right: string) =
    if (left.Length = 0 || right.Length = 0) then
        0
    else
        match (Seq.head left), (Seq.head right) with
        | a, b when a = b -> compareCardWise (left.Substring 1) (right.Substring 1)
        | a, b -> CardValues.Item a - CardValues.Item b

[<Struct; CustomComparisonAttribute; CustomEqualityAttribute>]
type Hand(input: string) =
    member this.cards = input

    member this.rank =
        this.cards
        |> Seq.fold
            (fun state c ->
                state
                |> Map.change c (fun f ->
                    match f with
                    | Some(v) -> (v + 1) |> Some
                    | None -> 1 |> Some))
            (Map<char, int>([]))
        |> getRankWithJokers

    override this.Equals obj =
        match obj with
        | :? Hand as h when this.rank = h.rank -> compareCardWise this.cards h.cards = 0
        | _ -> false

    override this.GetHashCode() = this.cards.GetHashCode()

    override this.ToString() = (this.cards, this.rank).ToString()

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Hand as h ->
                match this.rank, h.rank with
                | l, r when l = r -> compareCardWise this.cards h.cards
                | l, r -> l - r
            | _ -> raise (ArgumentException())

let readInput file = File.ReadAllLines file
//
// let a = Hand("2Q2QJ")
// let b = Hand("JJJJJ")
// printfn "%A %A" a b

"./inputs/07.txt"
|> readInput
//
// @"32T3K 765
// T55J5 684
// KK677 28
// KTJJT 220
// QQQJA 483".Split("\n")
|> Array.map (fun s ->
    let parts = s.Split(" ")
    Hand(parts[0]), parts[1] |> uint)
|> Array.sortBy fst
|> Array.indexed
// |> printfn "%A"
|> Array.fold (fun state (idx, (_, points)) -> state + (points * ((idx |> uint) + 1u))) 0u
|> printfn "%A"
