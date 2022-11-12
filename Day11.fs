module Day11

open Util

let day11 =
    printfn "--Day 11--"

    let serial =
        "./InputDay11.txt"
        |> readLines
        |> Seq.head
        |> int

    let powerOf serial (x, y) = 
        let rackId = x + 10
        let powerLevel = rackId * y
        let boostedPowerLevel = (powerLevel + serial) * rackId 
        let hunderDigit = ((boostedPowerLevel % 1000) / 100)
        let power = hunderDigit - 5
        power

    let summedArea (areaMap: int[,]) square =
        let ((x, y), size) = square
        areaMap[x + size - 1, y + size - 1] -
        areaMap[x - 1, y + size - 1] -
        areaMap[x + size - 1, y - 1] +
        areaMap[x - 1, y - 1]


    let squaresOfSize size = 
        (
            { 1 .. 300 - size + 1},
            { 1 .. 300 - size + 1 }
        )
        ||> Seq.allPairs
        |> Seq.map (fun coordinate -> (coordinate, size))

    let areaMap = Array2D.zeroCreate 301 301
    for x in 1..300 do
        for y in 1..300 do
            areaMap[x, y] <-
                areaMap[x - 1, y] +
                areaMap[x, y - 1] - 
                areaMap[x - 1, y - 1] +
                powerOf serial (x, y)

    let sizeThreeSquares = squaresOfSize 3
    let anySizeSquares = 
        { 1 .. 300 }
        |> Seq.map squaresOfSize
        |> Seq.concat

    let bestSizeThreeSquare = sizeThreeSquares |> Seq.maxBy (summedArea areaMap)
    let bestAnySizeSquare = 
        anySizeSquares
        |> Seq.maxBy (summedArea areaMap)

    let ((x1, y1), _) = bestSizeThreeSquare
    let ((x2, y2), size2) = bestAnySizeSquare
    
    printfn "Answer part 1: %d,%d" x1 y1
    printfn "Answer part 2: %d,%d,%d" x2 y2 size2