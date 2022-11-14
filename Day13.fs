module Day13

open System

type Direction =
    | North
    | South
    | West
    | East

type Turn =
    | Left
    | Forward
    | Right

type Cart = {
    Direction: Direction
    Position: int * int
    NextTurn: Turn
}

type State = {
    Map: char[,]
    Carts: Cart seq
}

type Result =
    | State of State
    | Collision of (int * int)

open Util

let day13 =
    printfn "--Day 13--"

    let lines =
        "InputDay13.txt"
        |> readLines

    let parseCart (x, y, v) =
        match v with
        | '^' -> Some North
        | 'v' -> Some South
        | '<' -> Some West
        | '>' -> Some East
        | _ -> None
        |> Option.map (fun direction -> {
            Direction = direction
            Position = x, y
            NextTurn = Left })

    let width = lines |> Seq.head |> Seq.length
    let height = lines |> Seq.length

    let map =
        Array2D.init width height (fun x y ->
            lines
            |> Seq.transpose
            |> Seq.item x
            |> Seq.item y)

    let carts =
        map
        |> Array2D.mapi (fun x y v -> (x, y, v))
        |> Seq.cast<int * int * char>
        |> Seq.choose parseCart

    carts
    |> Seq.iter (fun cart ->
        map[fst cart.Position, snd cart.Position] <-
            match cart.Direction with
            | North
            | South -> '|'
            | West
            | East  -> '-')

    let doTurn direction turn =
        match direction, turn with
        | _, Forward -> direction
        | North, Left -> West
        | East, Left -> North
        | South, Left -> East
        | West, Left -> South
        | North, Right -> East
        | East, Right -> South
        | South, Right -> West
        | West, Right -> North

    let nextTurn turn =
        match turn with
        | Left -> Forward
        | Forward -> Right
        | Right -> Left

    let printMap map =
        let width = map |> Array2D.length1
        let height = map |> Array2D.length2
        for y in 0 .. height - 1  do
            map[0 .. width - 1, y]
            |> String
            |> printfn "%s"

    let moveCart state cartIndex =
        let cart = state.Carts |> Seq.item cartIndex

        let nextPosition =
            let dx, dy =
                match cart.Direction with
                | North -> 0, -1
                | South -> 0, 1
                | West -> -1, 0
                | East  -> 1, 0

            (fst cart.Position + dx, snd cart.Position + dy)

        let nextType = state.Map[fst nextPosition, snd nextPosition]

        let nextDirection =
            match cart.Direction, nextType with
            | North, '\\' -> West
            | North, '/' -> East
            | South, '\\' -> East
            | South, '/' -> West
            | West, '\\' -> North
            | West, '/' -> South
            | East , '\\' -> South
            | East , '/' -> North
            | direction, '+' -> doTurn direction cart.NextTurn
            | _ -> cart.Direction

        let nextCart = {
            Direction = nextDirection
            Position = nextPosition
            NextTurn = if nextType = '+' then nextTurn cart.NextTurn else cart.NextTurn
        }

        { state with
            Carts = state.Carts |> Seq.updateAt cartIndex nextCart }

    let getCollision state =
        state.Carts
        |> Seq.groupBy (fun cart -> cart.Position)
        |> Seq.tryFind (fun (_, group) -> group |> Seq.length > 1)
        |> Option.map fst


    let tick state =

        let sortedState =
            { state with
                Carts =
                state.Carts
                    |> Seq.sortBy (fun cart -> fst cart.Position)
                    |> Seq.sortBy (fun cart -> snd cart.Position) }

        let cartCount = state.Carts |> Seq.length

        (State sortedState, { 0 .. cartCount - 1 })
        ||> Seq.fold (fun result index ->
            match result with
            | State state ->
                let nextState = moveCart state index
                let collision = getCollision nextState

                match collision with
                | Some position -> Collision position
                | None -> State nextState
            | Collision position -> Collision position
        )

    let initialState = {
        Map = map
        Carts = carts
    }

    let mutable state = initialState
    let mutable collision = None

    while Option.isNone collision do
        let result = tick state
        match result with
        | State s -> state <- s
        | Collision position -> collision <- Some position

    printfn "%A" collision