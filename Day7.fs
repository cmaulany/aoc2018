module Day7

open System
open System.Text.RegularExpressions
open Util

type Time = int
type Step = char

type Rule = {
    Step: Step
    Requirements: Step list
}

type Task = {
    Step: Step
    Start: Time
    End: Time
}

type State = {
    Time: int
    Rules: Rule list
    PendingSteps: Step list
    CompletedSteps: Step list
    Tasks: Task list
    AvailableWorkers: int
}


let day7 =
    let lines = 
        "./InputDay7.txt"
        |> readLines
        |> Seq.map (fun line -> 
            let groups = 
                (line, @"Step (\w) must be finished before step (\w) can begin\.")
                |> Regex.Match
                |> fun m -> Seq.tail m.Groups 
                |> Seq.map (fun group -> char group.Value)
                |> Seq.toList
            (groups[0], groups[1]))
    
    let steps: seq<Step> = 
        lines
        |> Seq.collect (fun (a, b) -> [a; b])
        |> Seq.distinct
        |> Seq.sort

    let rules = 
        lines
        |> Seq.groupBy snd
        |> Seq.map (fun (step, group) -> 
            {
                Step = step
                Requirements = 
                    group 
                    |> Seq.map fst
                    |> Seq.toList
            }
        )
        |> Seq.sortBy (fun rule -> rule.Step)

    let findRuleForStep (rules: seq<Rule>) step =
        rules
        |> Seq.tryFind (fun rule -> rule.Step = step)

    let getReadySteps state = 
        state.PendingSteps
        |> Seq.filter (fun step -> 
            match findRuleForStep state.Rules step with
            | Some(rule) -> 
                rule.Requirements
                |> Seq.forall (fun requirement -> 
                    Seq.contains requirement state.CompletedSteps)
            | None -> true)

    let completeTasks state = 
        let completedTasks = 
            state.Tasks
            |> Seq.filter (fun task -> task.End <= state.Time)

        let completedSteps = 
            completedTasks
            |> Seq.map (fun task -> task.Step)

        { state with
            Tasks = state.Tasks |> List.except completedTasks
            CompletedSteps = state.CompletedSteps @ Seq.toList completedSteps
            AvailableWorkers = state.AvailableWorkers + Seq.length completedSteps }

    let startTasks state =
        let readySteps = getReadySteps state

        let newTaskCount = [Seq.length readySteps; state.AvailableWorkers] |> Seq.min

        let newSteps = readySteps |> Seq.take newTaskCount

        let newTasks = 
            newSteps
            |> Seq.map (fun step ->
                {
                    Step = step
                    Start = state.Time
                    End = state.Time + 61 + int step - int 'A'
                })

        { state with
            Tasks = state.Tasks @ Seq.toList newTasks
            PendingSteps = state.PendingSteps |> List.except newSteps
            AvailableWorkers = state.AvailableWorkers - Seq.length newTasks }

    let progressToNextCompletedTask state =
        let nextTime = 
            state.Tasks
            |> Seq.sortBy (fun task -> task.End)
            |> Seq.tryPick (fun task -> 
                if task.End >= state.Time
                    then Some(task.End)
                    else None)
        
        match nextTime with
        | Some(nextTime) -> { state with Time = nextTime }
        | None -> state

    let hasFinished state = 
        Seq.length state.Tasks = 0 && 
        Seq.length state.PendingSteps = 0


    let tick =
        startTasks
        >> progressToNextCompletedTask
        >> completeTasks


    let rec loop state =
        let nextState = tick state
        if hasFinished state
            then nextState
            else loop nextState
            
    let statePart1 = {
        Time = 0
        Rules = rules |> Seq.toList
        PendingSteps = steps |> Seq.toList
        CompletedSteps = []
        Tasks = []
        AvailableWorkers = 1
    }

    let statePart2 = {
        Time = 0
        Rules = rules |> Seq.toList
        PendingSteps = steps |> Seq.toList
        CompletedSteps = []
        Tasks = []
        AvailableWorkers = 5
    }

    let finalStatePart1 = loop statePart1
    let finalStatePart2 = loop statePart2


    printfn "Answer part 1: %s" (finalStatePart1.CompletedSteps |> String.Concat)
    printfn "Resutl part 2: %d" finalStatePart2.Time
        

    