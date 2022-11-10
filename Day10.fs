module Day10

open Util
open System.Text.RegularExpressions
open System

type Vector2 = int * int

type Light = {
    Position: Vector2
    Velocity: Vector2
}

let day10 =
    let lights = 
        "./InputDay10.txt"
        |> readLines
        |> Seq.map (fun line -> 
            let numbers = 
                (line, @"position=< *(-?\d+), *(-?\d+)> velocity=< *(-?\d+), *(-?\d+)>")
                |> Regex.Match
                |> fun m -> Seq.tail m.Groups
                |> Seq.map (fun group -> int group.Value)
                |> Seq.toList
            { 
                Position = numbers[0], numbers[1]
                Velocity = numbers[2], numbers[3]
            })

    let tick lights steps = 
        lights
        |> Seq.map (fun light ->
            { light with
                Position = (
                    fst light.Position + fst light.Velocity * steps,
                    snd light.Position + snd light.Velocity * steps
                )
            }
        )

    let printLights lights =
        let positions = 
            lights 
            |> Seq.map (fun light -> light.Position)

        let minX = positions |> Seq.map fst |> Seq.min
        let maxX = positions |> Seq.map fst |> Seq.max
        let minY = positions |> Seq.map snd |> Seq.min
        let maxY = positions |> Seq.map snd |> Seq.max

        let positionMap =
            positions
            |> Seq.map (fun position -> (position, ()))
            |> Map.ofSeq

        for y in minY..maxY do
            { minX..maxX }
            |> Seq.map (fun x -> 
                if positionMap |> Map.containsKey (x, y) 
                    then '#' 
                    else '.')
            |> String.Concat
            |> printfn "%s"

    let height lights = 
        let positions = 
            lights 
            |> Seq.map (fun light -> light.Position)

        let minY = positions |> Seq.map snd |> Seq.min
        let maxY = positions |> Seq.map snd |> Seq.max

        maxY - minY

    printfn "--Day 10--"

    let mutable i = 0
    let mutable interval = 100
    while 
        height (tick lights (i + interval)) > 
        height (tick lights (i + 2 * interval)) do
        i <- i + interval

    while height (tick lights i) > height (tick lights (i + 1)) do
        i <- i + 1

    printfn "Tick %d" i
    printLights (tick lights i)
    