module Day2

open Util

let day2 =
    let boxIds = readLines "./InputDay2.txt"

    let getCounts boxId = 
        let counts = boxId |> Seq.countBy id
        let hasTwo = Seq.exists (fun count -> snd count = 2) counts
        let hasThree = Seq.exists (fun count -> snd count = 3) counts
        (if hasTwo then 1 else 0), (if hasThree then 1 else 0)

    
    let counts: seq<int * int> = boxIds |> Seq.map getCounts

    let twoCount = counts |> Seq.map fst |> Seq.sum
    let threeCount = counts |> Seq.map snd |> Seq.sum
    let resultPart1 = twoCount * threeCount

    let overlap a b = 
        Seq.zip a b 
        |> Seq.filter (fun (a, b) -> a = b) 
        |> Seq.map fst

    let resultPart2 = 
        (boxIds, boxIds) 
        ||> Seq.allPairs 
        |> Seq.pick (fun (a, b) -> 
            let m = overlap a b
            if (Seq.length a) - (Seq.length m) = 1
                then Some(m)
                else None
        )
        |> System.String.Concat

    printfn "--Day 2--"
    printfn "Part 1: %d" resultPart1
    printfn "Part 2: %s" resultPart2
