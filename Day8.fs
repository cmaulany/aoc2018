module Day8

open System
open Util

type Node = {
    Children: Node list
    MetadataEntries: int list
}

let day8 =
    let numbers =
        "./InputDay8.txt"
        |> readLines
        |> Seq.head
        |> fun line -> line.Split([|' '|])
        |> Seq.map int
        |> Seq.toArray

    let mutable i = 0;

    let rec parseNode () =
        let childCount = numbers[i]
        let metadataCount = numbers[i + 1]

        i <- i + 2

        let children = [ for _ in 1 .. childCount -> parseNode () ]

        let start = i
        i <- i + metadataCount

        {
            Children = children
            MetadataEntries = numbers[ start .. i - 1 ] |> Array.toList
        }

    let rec sumMetadata node =
        Seq.sum node.MetadataEntries +
        Seq.sumBy sumMetadata node.Children

    let rec calculateValue node =
        if Seq.isEmpty node.Children then
            Seq.sum node.MetadataEntries
        else
            node.MetadataEntries
            |> Seq.map ((+) -1)
            |> Seq.filter (fun index -> index < Seq.length node.Children)
            |> Seq.sumBy (fun index ->
                node.Children
                |> Seq.item index
                |> calculateValue)

    let root = parseNode ()

    let rootSum = sumMetadata root
    let rootValue = calculateValue root


    printfn "--Day 8--"
    printfn "Result part 1: %A" rootSum
    printfn "Result part 2: %A" rootValue
