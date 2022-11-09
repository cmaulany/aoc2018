
module Day1

open Util

let day1 =
    let lines = readLines "./InputDay1.txt"
    let numbers = lines |> Seq.map int

    let rec repeat sequence = seq {
        yield! sequence
        yield! repeat sequence
    }

    let toFrequencies numbers = (0, numbers) ||> Seq.scan (+)

    let firstDuplicate numbers = 
        let mutable visited = Set.empty
        numbers |> Seq.pick (fun number -> 
            if Set.contains number visited 
                then Some(number)
                else 
                    visited <- Set.add number visited
                    None
        )

    let resultPart1 = numbers |> toFrequencies |> Seq.last
    let resultPart2 = repeat numbers |> toFrequencies |> firstDuplicate

    printfn "--Day 1--"
    printfn "Part 1: %d" resultPart1
    printfn "Part 2: %A" resultPart2