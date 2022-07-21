module Avalon.Quests
open Teams
open Voting

type QuestTemplate = {
    population: int
}

type Quest = {
    proposedBy: Player
    members: Player list
    name: string
}

let nextQuestTemplateFrom playerBase history =
    match List.length playerBase.players with
    | 5 -> 
        match List.length history with
        | 0 -> { population = 2 }
        | 1 -> { population = 3 }
        | 2 -> { population = 2 }
        | 3 -> { population = 3 }
        | _ -> { population = 3 }
    | 6 ->
        match List.length history with
        | 0 -> { population = 2 }
        | 1 -> { population = 3 }
        | 2 -> { population = 4 }
        | 3 -> { population = 3 }
        | _ -> { population = 4 }
    | 7 ->
        match List.length history with
        | 0 -> { population = 2 }
        | 1 -> { population = 3 }
        | 2 -> { population = 3 }
        | 3 -> { population = 4 }
        | _ -> { population = 4 }
    | _ ->
        match List.length history with
        | 0 -> { population = 3 }
        | 1 -> { population = 4 }
        | 2 -> { population = 4 }
        | 3 -> { population = 5 }
        | _ -> { population = 5 }

let proposeAQuest getAQuestName getAMember template team =
    let everyone = team.nextRotation |> Set.ofList
    let rec findMembers currentMembers =
        let chosenMember =
            currentMembers
                |> Set.difference everyone
                |> getAMember
        let newMembership = Set.add chosenMember currentMembers
        match Set.count newMembership with
        | full when full = template.population -> newMembership |> Set.toList
        | _ -> findMembers newMembership
    {
        proposedBy = team.leader
        members = findMembers Set.empty
        name = getAQuestName template
    }

type QuestVotingState = {
    record: VotingRecord
    quest: Quest
    votesTaken: int
}

type QuestVoteResults =
    | NoVotesYet
    | Rejected of VotingRecord * int
    /// Once it has been Accepted it shouldn't be voted on again
    | Accepted of VotingRecord * int
    /// Once Evil is victorious it shouldn't be voted on again
    | EvilVictory of VotingRecord

let voteOnAQuest getAVote team state =
    match state with
    | NoVotesYet ->
        let record = voteUntilDone getAVote team Map.empty
        match determineOutcome record with
        | QuestYea -> Accepted (record, 1)
        | QuestNay -> Rejected (record, 1)
    | Rejected (_, v) -> 
        let record = voteUntilDone getAVote team Map.empty
        let thisVoteWas = v + 1
        match determineOutcome record with
        | QuestYea -> Accepted (record, thisVoteWas)
        | QuestNay ->
            match thisVoteWas with
            | 5 -> EvilVictory record
            | _ -> Rejected (record, thisVoteWas)
    | Accepted _ | EvilVictory _ -> state

type QuestResult =
    | QuestSuccess
    | QuestFailure

let presentPossibleResults character =
    match character with
    | Good | Merlin -> [QuestSuccess]
    | Evil | TheAssassin -> [QuestSuccess; QuestFailure]


type QuestHistory = {
    quest: Quest
    result: QuestResult
}

let tabulateQuestResult playerBase history results =
    let failuresRequired = 
        match List.length history, List.length playerBase.players with
        // The 4th quest (3 quests have passed) takes 2 failures when there are more than 6 players
        | (3, 7) | (3, 8) | (3, 9) | (3, 10) -> 2
        | _ -> 1
    let failuresPresent =
        results
        |> List.filter (fun r -> r = QuestFailure)
        |> List.length
    if failuresPresent < failuresRequired then
        QuestSuccess
    else
        QuestFailure

let determineQuestFate playerBase getAResult history quest =
    let rec getResults priorResults =
        let (whoVoted, thisResult) = getAResult (quest.members |> Seq.except (Map.keys priorResults))
        let newResults = priorResults |> Map.add whoVoted thisResult
        if Map.count newResults = List.length quest.members then
            tabulateQuestResult playerBase history (Map.values newResults |> List.ofSeq)
        else
            getResults newResults 
    {
        quest = quest
        result = getResults Map.empty
    }