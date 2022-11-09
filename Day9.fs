module Day9

type Marble = int

type Player = {
    Score: int
}

type State = {
    Circle: Marble list
    Current: Marble
    Players: Player list
}

let day9 = 

    let playerCount = 441

    let initialState = {
        Circle = [0]
        Current = 0
        Players = [ 
            for _ in 1..playerCount -> 
            {
                Score = 0
            }
        ]
    }

    let finalState = 
        (initialState, seq { 1 .. 71032 })
        ||> Seq.fold (fun (state: State) i -> 
            let current = state.Current
            let next = i

            let currentIndex =
                state.Circle
                |> Seq.findIndex (fun marble -> marble = current) 

            let circleSize = Seq.length state.Circle

            if next % 23 = 0 then
                let sevenCcIndex = (currentIndex - 7 + circleSize) % circleSize 
                let sevenCc = 
                    state.Circle
                    |> Seq.item sevenCcIndex


                let newCircle =
                    state.Circle
                    |> List.except [sevenCc; next]

                let newPosition = sevenCcIndex % (circleSize - 1)

                let newCurrent = 
                    newCircle
                    |> Seq.item newPosition

                let score = sevenCc + next
                let playerIndex = (i - 1) % (Seq.length state.Players)

                let player =
                    state.Players
                    |> Seq.item playerIndex

                let players = 
                    state.Players
                    |> List.updateAt playerIndex { Score = player.Score + score}

                { state with
                    Circle = newCircle
                    Current = newCurrent
                    Players = players
                }
            else 
                let newPosition = (currentIndex + 2) % circleSize 
                
                let nextCircle = 
                    state.Circle
                    |> List.insertAt newPosition next

                // printfn "%d - %A (%d)" current nextCircle next
                { state with
                    Circle = nextCircle
                    Current = next
                }
        )

    printfn "--Day 9--"

    let winner = 
        finalState.Players
        |> Seq.map (fun player -> player.Score)
        |> Seq.max

    printfn "Answer part 1: %d" winner