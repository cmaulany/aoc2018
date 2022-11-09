module Day9

type Node(value: int) as this =
    member val Previous = this with get,set
    member val Next = this with get,set
    
    member val Value = value

    member this.Rotate (n) =
        (this, { 1 .. abs n})
        ||> Seq.fold (fun node _ -> 
            if n > 0 
                then node.Next 
                else node.Previous
        )

    member this.Insert (value) = 
        let newNode = new Node(value)
        let previous = this.Previous
        previous.Next <- newNode
        newNode.Previous <- previous
        newNode.Next <- this
        this.Previous <- newNode
        newNode

    member this.Remove () =
        let previous = this.Previous
        let next = this.Next
        previous.Next <- next
        next.Previous <- previous
        next

    member this.Collect () =
        let mutable result = [this]
        let mutable node = this.Previous
        while node <> this do
            result <- node::result
            node <- node.Previous
        result
        |> List.map (fun n -> n.Value)
        
type GameState = {
    Circle: Node
    PlayerCount: int
    Scores: Map<int, int64>
}

let node = new Node(0)

let day9 = 
    let run playerCount marbleCount = 
        let state = {
            Circle = Node(0)
            PlayerCount = playerCount
            Scores = Map.empty
        }

        (state, { 1..marbleCount })
        ||> Seq.fold (fun state i -> 
            if i % 23 = 0 then
                let toTake = state.Circle.Rotate(-7)

                let playerId = (i % state.PlayerCount) - 1
                let previousScore =
                    match state.Scores |> Map.tryFind playerId with
                    | None -> int64 0
                    | Some(score) -> score
                let newScore = previousScore + int64 toTake.Value + int64 i
                let scores = state.Scores |> Map.add playerId newScore

                { state with
                    Circle = toTake.Remove() 
                    Scores = scores }
            else
                { state with 
                    Circle = state.Circle.Rotate(2).Insert(i) }
        )

    let gamePart1 = run 441 71032
    let gamePart2 = run 441 (71032 * 100)

    let maxScore state = 
        state.Scores
        |> Map.values
        |> Seq.max

    let resultPart1 = maxScore gamePart1
    let resultPart2 = maxScore gamePart2

    printfn "--Day 9--"
    printfn "Result part 1: %d" resultPart1
    printfn "Result part 2: %d" resultPart2