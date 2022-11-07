module Day7

open System.Text.RegularExpressions
open Util

type Rule = {
    Step: char
    Requirements: char list
}

let day7 =
    printfn "--Day 7--"

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
            (groups[0], groups[1])
        )
    
    let steps = 
        lines
        |> Seq.collect (fun (a, b) -> [a; b] )
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

    let findRuleForStep rules step =
        rules
        |> Seq.tryFind (fun rule -> rule.Step = step)
        
    let getNextState (steps, rules, sequence) =
        let currentStep = 
            steps
            |> Seq.find (findRuleForStep rules >> Option.isNone)
        let nextSteps = steps |> Seq.except [currentStep]
        let nextRules = 
            rules
            |> Seq.filter (fun rule -> 
                rule.Requirements 
                |> Seq.exists (fun requirement -> 
                    Seq.contains requirement nextSteps
                )
            )
        (nextSteps, nextRules, sequence + string currentStep)

    let initialState = (steps, rules, "")
    
    let mutable state = initialState
    for _ in 1 .. Seq.length steps do
        state <- getNextState state

    let (_, _, sequence) = state

    printfn "Answer part 1: %s" sequence