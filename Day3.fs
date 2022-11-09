module Day3

open System.Text.RegularExpressions
open Util

type Id = int

type Claim = {
    Id: Id
    X: int
    Y: int
    Width: int
    Height: int
}

type Coordinate = {
    X: int
    Y: int
    Claims: Claim List
}

let day3 =
    let lines = readLines "./InputDay3.txt"
    
    let claims = lines |> Seq.map (fun line -> 
        let numbers = 
            (line, @"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)") 
            |> Regex.Match
            |> fun m -> Seq.tail m.Groups
            |> Seq.map (fun group -> int group.Value)
            |> Seq.toList
        {
            Id = numbers[0]
            X = numbers[1]
            Y = numbers[2]
            Width = numbers[3]
            Height = numbers[4]
        }
    )

    let claimToSquares claim = seq {
        for dx in 1 .. claim.Width do
            for dy in 1 .. claim.Height ->
                ((claim.X + dx, claim.Y + dy), claim)
    }

    let coordinates = 
        claims
        |> Seq.collect claimToSquares
        |> Seq.groupBy fst
        |> Seq.map (fun ((x, y), grouped) -> 
            {
                X = x
                Y = y
                Claims = grouped |> Seq.map snd |> Seq.toList
            }
        )

    let overlappingCoordinates = 
        coordinates
        |> Seq.filter (fun coordinate -> Seq.length coordinate.Claims >= 2)

    let overlapCount = overlappingCoordinates |> Seq.length

    let overlappingClaims = 
        overlappingCoordinates 
        |> Seq.collect (fun coordinate -> coordinate.Claims)
        |> Set.ofSeq

    let nonOverlappingClaim = 
        claims
        |> Seq.collect claimToSquares
        |> Seq.find (fun square -> not (Seq.contains (snd square) overlappingClaims))
        |> snd
        
    printfn "--Day 3--"
    printfn "Part 1: %A" overlapCount
    printfn "Part 2: %A" nonOverlappingClaim.Id