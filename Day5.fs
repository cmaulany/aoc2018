module Day5

open System
open Util

type Polarity = 
    | High
    | Low

type Unit = {
    Type: char
    Polarity: Polarity
}

let day5 =
    let input = 
        "./InputDay5.txt"
        |> readLines  
        |> Seq.head
        |> Seq.map (fun c -> 
            {
                Type = Char.ToUpper c
                Polarity = if Char.IsUpper c then High else Low
            }
        )

    let react a b = 
        if a.Type = b.Type && a.Polarity <> b.Polarity
            then []
            else [a; b]

    let reducePolymer polymer = 
        (polymer, [])
        ||> Seq.foldBack (fun unit polymer -> 
            match polymer with
            | head::tail -> react unit head @ tail
            | [] -> [unit]
        )
    
    let reducedInput = reducePolymer input
    let reducedInputLength = List.length reducedInput

    let unitTypes =
        input
        |> Seq.map (fun unit -> unit.Type)
        |> Seq.distinct

    let filterType polymer t =
        polymer
        |> Seq.filter (fun unit -> unit.Type <> t)

    let smallestPolymer =
        unitTypes
        |> Seq.map (filterType input)
        |> Seq.map reducePolymer
        |> Seq.minBy Seq.length

    let smallestPolymerLength = Seq.length smallestPolymer

    printfn("--Day 5--")
    printfn "Result part 1: %A" reducedInputLength
    printfn "Result part 2: %A" smallestPolymerLength