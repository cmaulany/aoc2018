module Day6

open System.Text.RegularExpressions
open System
open Util

type Coordinate = {
    X: int
    Y: int
}

let day6 =
    let coordinates = 
        "./InputDay6.txt" 
        |> readLines
        |> Seq.map (fun line -> 
            let groups = 
                (line, @"(\d+), (\d+)")
                |> Regex.Match
                |> fun m -> Seq.tail m.Groups 
                |> Seq.map (fun group -> group.Value)
                |> Seq.toList
            {
                X = int groups[0]
                Y = int groups[1] 
            }
        )

    let getDistance a b =
        Math.Abs (b.X - a.X) +
        Math.Abs (b.Y - a.Y)

    let minX = coordinates |> Seq.map (fun coordinate -> coordinate.X) |> Seq.min
    let maxX = coordinates |> Seq.map (fun coordinate -> coordinate.X) |> Seq.max
    let minY = coordinates |> Seq.map (fun coordinate -> coordinate.Y) |> Seq.min
    let maxY = coordinates |> Seq.map (fun coordinate -> coordinate.Y) |> Seq.max

    let allCoordinates = seq {
        for x in minX .. maxX do
            for y in minY .. maxY -> {
                    X = x
                    Y = y
                }
    }

    let getClosestCoordinate coordinates coordinate = 
        let closestCoordinates = 
            coordinates
            |> Seq.groupBy (getDistance coordinate)
            |> Seq.minBy fst
            |> snd
        if Seq.length closestCoordinates = 1
            then Some (Seq.head closestCoordinates)
            else None

    let areas =
        allCoordinates
        |> Seq.groupBy (getClosestCoordinate coordinates)
        |> Seq.filter (fst >> Option.isSome)
        |> Seq.map snd

    let nonInfiniteAreas =
        areas
        |> Seq.filter (fun claim -> 
            Seq.isEmpty claim ||
            claim
            |> Seq.forall (fun coordinate -> 
                coordinate.X <> minX &&
                coordinate.X <> maxX &&
                coordinate.Y <> minY &&
                coordinate.Y <> maxY))

    let largestFiniteArea = nonInfiniteAreas |> Seq.maxBy Seq.length
    let largestFiniteAreaSize = largestFiniteArea |> Seq.length

    let allCoordinatesInRange =
        allCoordinates
        |> Seq.map (fun coordinate -> 
            coordinates 
            |> Seq.sumBy (getDistance coordinate))
        |> Seq.filter (fun n -> n < 10000)
    let allCoordinatesInRangeCount = Seq.length allCoordinatesInRange

    printfn "--Day 6--"
    printfn "Answer part 1: %d" largestFiniteAreaSize
    printfn "Answer part 2: %d" allCoordinatesInRangeCount