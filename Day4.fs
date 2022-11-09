module Day4

open System.Text.RegularExpressions
open System
open Util

type Timestamp = DateTime

type StartShift = {
    GuardId: int
}

type Event = 
    | StartShift of StartShift
    | FallAsleep
    | WakeUp

type Record = {
    Timestamp: DateTime
    Event: Event
}

type Sleep = {
    Start: DateTime
    End: DateTime
}

type Shift = {
    Date: DateTime
    GuardId: int
    Sleeps: Sleep list
}

type GuardSchedule = {
    GuardId: int
    Shifts: Shift list
}

let day4 = 
    let lines = readLines "./InputDay4.txt"

    let records =
        lines
        |> Seq.map (fun line ->
            let groups = 
                (line, @"\[(\S+ \S+)] (.+)")
                |> Regex.Match
                |> fun m -> Seq.tail m.Groups 
                |> Seq.map (fun group -> group.Value)
                |> Seq.toList
            {
                Timestamp =  groups[0] |> DateTime.Parse 
                Event = 
                    match groups[1] with
                    | "wakes up" -> WakeUp
                    | "falls asleep" -> FallAsleep
                    | startShift -> 
                        (startShift, @"Guard #(\d+) begins shift") 
                        |> Regex.Match
                        |> fun m -> 
                            StartShift {
                                GuardId = (Seq.item 1 m.Groups).Value |> int
                            }
            }
        )
        |> Seq.sortBy (fun record -> record.Timestamp)

    let timeStampToDate (timestamp: DateTime) = 
        if timestamp.Hour = 23
            then timestamp.AddDays(1).Date
            else timestamp.Date

    let shifts =
        records
        |> Seq.groupBy (fun record -> timeStampToDate record.Timestamp)
        |> Seq.map (fun (date, records) -> 
            let rec recordsToSleeps records = 
                match records with
                | first:: rest -> 
                    {
                        Start = first.Timestamp
                        End = rest.Head.Timestamp
                    }
                    ::recordsToSleeps rest.Tail
                | _ -> []

            let (startShift, guardId) = 
                records
                |> Seq.pick (fun record -> 
                    match record with
                    | { Event = StartShift startShift } -> Some (record, startShift.GuardId)
                    | _ -> None
                )
            let rest = 
                records
                |> Seq.filter (fun record ->
                    record <> startShift)
            {
                Date = date
                GuardId = guardId
                Sleeps = rest |> Seq.toList |> recordsToSleeps 
            }
        )

    let shiftToMinutesAsleep shift =
        shift.Sleeps 
        |> List.sumBy (fun sleep -> (sleep.End - sleep.Start).Minutes)

    let guardSchedules = 
        shifts 
        |> Seq.groupBy (fun shift -> shift.GuardId)
        |> Seq.map (fun (guardId: int, shifts) -> 
            {
                GuardId = guardId
                Shifts = Seq.toList shifts
            })

    let mostSleptSchedule =
        guardSchedules
        |> Seq.maxBy (fun guardSchedule -> 
            guardSchedule.Shifts
            |> Seq.sumBy shiftToMinutesAsleep)

    let sleepingMinutesInShift shift =
        shift.Sleeps
        |> Seq.collect (fun sleep -> seq {
            for minute in sleep.Start.Minute .. sleep.End.Minute - 1 -> minute
        })

    let getMostSleptMinute shifts =
        shifts
        |> Seq.collect sleepingMinutesInShift
        |> fun minutes -> 
            if Seq.isEmpty minutes
                then None
                else Some minutes
        |> Option.map (fun minutes -> 
            minutes
            |> Seq.groupBy id
            |> Seq.maxBy (snd >> Seq.length))

    let mostSleptMinute: int = 
        mostSleptSchedule.Shifts 
        |> getMostSleptMinute
        |> Option.get 
        |> fst

    let mostOverlappingSchedule = 
        guardSchedules
        |> Seq.maxBy (fun schedule -> 
            schedule.Shifts
            |> getMostSleptMinute
            |> Option.map (snd >> Seq.length)
            |> Option.defaultWith (fun () -> 0))

    let mostOverlappingMinute = 
        mostOverlappingSchedule.Shifts 
        |> getMostSleptMinute 
        |> Option.get
        |> fst

    let answerPart1 = mostSleptSchedule.GuardId * mostSleptMinute
    let answerPart2 = mostOverlappingSchedule.GuardId * mostOverlappingMinute

    printfn "--Day 4--"
    printfn "Answer part 1: %d * %d = %d" mostSleptSchedule.GuardId mostSleptMinute answerPart1 
    printfn "Answer part 2: %d * %d = %d" mostOverlappingSchedule.GuardId mostOverlappingMinute answerPart2
