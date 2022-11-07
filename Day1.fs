
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

    // let mySet = Seq.initInfinite id |> Set.ofSeq
    // printfn "hey %A" (Set.contains 10 mySet)

    // let duplicates numbers =
    //     numbers 
    //     |> Seq.indexed
    //     |> Seq.where(fun (index, number) -> 
    //         printfn "%A" (index)
    //         Seq.take index numbers |> Set.ofSeq |> Set.contains number
    //     ) 

    let resultPar1 = numbers |> toFrequencies |> Seq.last
    let resultPart2 = repeat numbers |> toFrequencies |> firstDuplicate

    printfn "--Day 1--"
    printfn "Part 1: %d" resultPar1
    printfn "Part 2: %A" resultPart2