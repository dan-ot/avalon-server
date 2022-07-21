module Avalon.Game
open Teams
open Quests

type GamePhases =
    | FormATeam
    | DecideAQuest
    | GoQuesting
    | ResolveTheClimax

type GameResult =
    | StillPlaying of QuestHistory list
    | GoodVictory
    | EvilVictory

let determineGameResult questHistory =
    if questHistory |> List.filter (fun h -> h.result = QuestSuccess) |> List.length = 3 then
        GoodVictory
    else if questHistory |> List.filter (fun h -> h.result = QuestFailure) |> List.length = 3 then
        EvilVictory
    else
        StillPlaying questHistory

let prepareAssassination participants =
    let (assassin, _) = 
        participants
        |> Map.toSeq
        |> Seq.filter (fun (_, c) -> c = TheAssassin)
        |> Seq.head
    
    let goodGuys =
        participants
        |> Map.toSeq
        |> Seq.filter (fun (_, c) -> c = Merlin || c = Good)

    (assassin, goodGuys)