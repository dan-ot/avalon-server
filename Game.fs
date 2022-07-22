module Avalon.Game
open Teams
open Quests
open Ops

type FinishedQuesting =
    | GoodMajority
    | GoodInaction
    | EvilMajority

type QuestsResult =
    | StillQuesting of QuestHistory list
    | FinishedQuesting of FinishedQuesting

let determineQuestsResult questHistory =
    if questHistory |> List.filter (fun h -> h.result = QuestSuccess) |> List.length = 3 then
        FinishedQuesting GoodMajority
    else if questHistory |> List.filter (fun h -> h.result = QuestFailure) |> List.length = 3 then
        FinishedQuesting EvilMajority
    else
        StillQuesting questHistory

type AssassinationAttempt = {
    assassin: Player
    merlin: Player
    allTargets: Player list
}

let prepareAssassination rnd participants =
    let assassin = 
        participants
        |> whoIs TheAssassin
    
    let merlin =
        participants
        |> whoIs Merlin

    let goodGuys =
        participants
        |> whoAre [Good]
        |> List.ofSeq

    let allTargets = shuffle rnd (merlin :: goodGuys)

    { assassin = assassin; merlin = merlin; allTargets = allTargets}

type EvilVictory =
    | Quests
    | Inaction
    | Assassination

type GameResult =
    | GoodVictory
    | EvilVictory of EvilVictory

let resolveGame pickATarget questsResult assassination =
    match questsResult with
    | EvilMajority -> EvilVictory Quests
    | GoodInaction -> EvilVictory Inaction
    | GoodMajority ->
        let assassinate = pickATarget assassination.allTargets
        if assassinate = assassination.merlin then
            EvilVictory Assassination
        else
            GoodVictory